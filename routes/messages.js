var mongoose    = require('mongoose');
var _prism_home   = process.env.PRISM_HOME;
var utils       = require(_prism_home + 'utils');
var PrismError  = require(_prism_home + 'error');
var _           = require('underscore');

var User            = mongoose.model('User');
var Organization    = mongoose.model('Organization');
var Message         = mongoose.model('Message');
var Trust           = mongoose.model('Trust');
var Group           = mongoose.model('Group');
var Activity        = mongoose.model('Activity');
var ObjectId        = require('mongoose').Types.ObjectId;
var shortFields = function(org) {
return {
    _id: 1, 
    name: 1, 
    profile_photo_url: 1,
    org_status: {
      $elemMatch: {
        organization: org._id,
        status: 'active'
      }
    }
  };
};

exports.fetchOrgs = function(req, res){
  var id = req.params.id;
  console.log(id);
  console.log('making request');
  User.findOne({_id: id})
  .populate({path: 'org_status.organization', model: 'Organization'})
  .populate({path: 'org_status.groups', model: 'Group'})
  .exec(function(err, user){
    console.log('received request');
    if (user){
      if (user.type == 'institution_verified') {
        Organization.findOne({owner: user._id})
        .populate({path: 'groups', model: 'Group'})
        .populate({path: 'theme', model: 'Theme'})
        .exec(function(err, result) {
          utils.prismResponse(res, [result], true);
        });
      } else {
        console.log('is regular user');
        var orgs = [];
        _.each(user.org_status, function(o, i, l){
           if (o.status == 'active' && o.organization){
             orgs.push(o.organization);
           } 
        });
        console.log(orgs);

        utils.prismResponse(res, orgs, true);
      }
    } else {
      console.log('no user');
      res.status(400).send();
    }
  });
};

exports.fetchGroups = function(req, res){
  var uid = req.params.user_id;
  var org_id = req.params.org_id;
  User.findOne({_id: uid})
  .populate({path: 'org_status.groups', model: 'Group'})
  .populate({path: 'org_status.organization', model: 'Organization'})
  .exec(function(err, user){
    if (user) {
      if (user.type == 'institution_verified'){
        Organization.findOne({owner: user._id})
        .populate({path: 'groups', model: 'Group'})
        .populate({path: 'theme', model: 'Theme'})
        .exec(function(err, result) {
          utils.prismResponse(res, result.groups, true);
        });
      } else {
        console.log('fetching groups');
        var groups;
        _.each(user.org_status, function(s, i, l){
          if (String(s.organization._id) == String(org_id) && s.status == 'active'){
            console.log(s.organization._id + '=' + org_id);
            groups = s.groups;
          }
        });
        console.log(groups);
        utils.prismResponse(res, groups, true);
      }
    } else {
      res.status(400).send();
    }
  });
}

exports.fetchGroupMembers = function(req, res){
  var o = req.params.org_id;
  var criteria = {
    org_status: {
      $elemMatch: {status: 'active', organization: ObjectId(o)}
    }
  };
  User.find(criteria)
  .populate({path: 'org_status.groups', model: 'Group'})
  .exec(function(err, users){
    if (err){
      console.log(err);
      res.status(500).send(err);
    } else {
      utils.prismResponse(res, users, true);
    }
  });
}

exports.fetchMessages = function(req, res){
  var group = req.params.group_name;
  var org_id = req.params.org_id;
  group = group == 'all'?null:group;
  var criteria = {organization: org_id, group: group};
  console.log('Since date: ' + req.query.since);
  if (req.query.since) {
    criteria.create_date = {$gt: req.query.since};
  }
    Message.find(criteria)
    .sort({create_date: -1})
    .exec(function(err, messages){
      if (err) {
        console.log(err);
        res.status(500).send(err);
      } else {
        console.log('done');
        utils.prismResponse(res, messages, true);
      }
    });

};

exports.updateMessage = function(req, res){
  var action = req.body.action;
  var uid = req.body.user;
  var messageID = req.params.message_id;
  if (action == 'like' || action == 'unlike'){
    if (messageID && uid) {
      Message.findOne({_id: messageID}, function(err, message){
        if (message) {
          console.log('found message');
          if (action == 'like') {
            console.log('liking message');
            message.likes.push(uid);
            message.likes_count += 1;
            message.save(function(err, res){
              if (err) console.log(err);
            });
            utils.prismResponse(res, message, true);
          } else {
            var idx = -1;
            _.each(message.likes, function(like, i, l){
              if (String(like) == String(uid)){
                idx = i;
              }
            });
            if (idx != -1) {
              message.likes.splice(idx, 1);
              message.likes_count -= 1;
              message.save(function(err, result){
                if (err) console.log(err);
              });  
            }
            utils.prismResponse(res, message, true);
          }
        } else {
          res.status(500).send(err);
        }
      });
    } else {
      res.status(400).send();
    }
  } 

}


exports.createMessage = function(req, res){
  var creator = req.body.creator;
  var text = req.body.text;
  var group = req.body.group=='all'?null:req.body.group;
  var organization = req.body.organization;

  var message = new Message({
    creator: creator, 
    text: text, 
    group: group, 
    organization: organization});
  message.save(function(err, result){
    if (err) {
      res.status(500).send(err);
    } else {
      utils.prismResponse(res, result, true);

    }
  });
}
