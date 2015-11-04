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
var ObjectIdRef     = mongoose.Schema.Types.ObjectId;
var request         = require('request');
var htmlparser      = require('htmlparser');
var Push            = require('../classes/PushNotification');

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
  User.findOne({_id: id})
  .populate({path: 'org_status.organization', model: 'Organization'})
  .populate({path: 'org_status.organization.owner', model: 'User', select: '_id name first_name last_name profile_photo_url type subtype'})
  .populate({path: 'org_status.groups', model: 'Group'})
  .exec(function(err, user){
    if (user){
      if (user.type == 'institution_verified') {
        Organization.find({owner: user._id})
        .populate({path: 'groups', model: 'Group'})
        .populate({path: 'theme', model: 'Theme'})
        .populate({path: 'owner', model: 'User', select: '_id name first_name last_name profile_photo_url type subtype'}) 
        .exec(function(err, result) {
          utils.prismResponse(res, result, true);
        });
      } else {
        var orgs = [];
        User.populate(user, {path: 'org_status.organization.theme', model: 'Theme'}, function(err, user){
          User.populate(user, {path: 'org_status.groups', model: 'Group'}, function(err, user){
            _.each(user.org_status, function(o, i, l){
               if (o.status == 'active' && o.organization){
                 orgs.push(o.organization);
               } 
            });

            utils.prismResponse(res, orgs, true);
          });
        });
      }
    } else {
      res.status(400).send();
    }
  });
};

exports.updateOrganization = function(req, res){
  var org_id = req.params.org_id;
  var action = req.body.action;
  var requestor = req.body.requestor;
  Organization.findOne({_id: org_id}, function(err, org){
    if (org){
      if (action) {
        if (action == 'mute'){
          var exists = false;
          _.each(org.mutes, function(id, i, l){
            console.log(id + '==' + requestor);
            if (String(id) == String(requestor)){
              exists = true;
            }
          });
          console.log(exists);
          if (!exists){
            console.log('muting');
            org.mutes.push(ObjectId(requestor));
            org.markModified('mutes');
            org.save(function(err, obj){});
          }
        } else if (action == 'unmute'){
          var idx = -1;
          _.each(org.mutes, function(id, i, l){
            if (String(id) == String(requestor)){
              idx = i;
            }
          });
          if (idx != -1) {
            org.mutes.splice(idx, 1);
            org.markModified('mutes');
            org.save(function(err, obj){});
          }
        }
        utils.prismResponse(res, org, true); 
      }
    } else {
      res.status(400).send();
    }
  });
}

exports.fetchGroups = function(req, res){
  var uid = req.params.user_id;
  var org_id = req.params.org_id;
  User.findOne({_id: uid})
  .populate({path: 'org_status.groups', model: 'Group'})
  .populate({path: 'org_status.organization', model: 'Organization'})
  .populate({path: 'org_status.organization.owner', model: 'User', select: '_id, first_name, last_name, profile_photo_url, name'})
  .exec(function(err, user){
    if (user) {
      if (user.type == 'institution_verified'){
        Group.find({organization: org_id, status: {$ne: 'inactive'}})
        .sort({name: 1})
        .exec(function(err, result) {
          utils.prismResponse(res, result, true);
        });
      } else {
        var groups;
        _.each(user.org_status, function(s, i, l){
          if(s.organization){
          if (String(s.organization._id) == String(org_id) && s.status == 'active'){
            groups = _.filter(s.groups, function(group){
              return group.status != 'inactive';
            });
          }
          }
        });
        groups = _.sortBy(groups, 'name');
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
    },
    active: true
  };
  User.find(criteria)
  .populate({path: 'org_status.groups', model: 'Group'})
  .select({_id: 1, name: 1, first_name: 1, last_name: 1,profile_photo_url: 1, org_status: 1, type: 1, subtype: 1, active: 1})
  .exec(function(err, users){
    if (err){
      console.log(err);
      res.status(500).send(err);
    } else {
      utils.prismResponse(res, users, true);
    }
  });
}

exports.getDirectMessages = function(req, res){
  var org_id = req.params.org_id;
  var target = req.params.uid;
  var user_id = req.query.requestor;
  var criteria = {$or: [{target: target, creator: user_id}, {creator: target, target: user_id}]};
  var sort = {create_date: -1};
  if (req.query.since) {
    criteria.create_date = {$gt: req.query.since};
    sort = {create_date: 1}
  }
  if (req.query.before) {
    criteria.create_date = {$lt: req.query.before};
    sort = {create_date: -1}
  }
  if (req.query.updated) {
    criteria.modify_date = {$gt: req.query.updated};
  }
  Message.find(criteria)
  .sort(sort)
  .limit(20)
  .exec(function(err, messages){
    if (err) {
      console.log(err);
      res.status(500).send(err);
    } else {
      if (!req.query.since) {
        messages = messages.reverse();
      }
      if (user_id){
        _.each(messages, function(m, i, l){
          var hasRead = false;
          _.each(m.read, function(r, id, l){
            console.log(r);
            if (String(r) == String(user_id)){
              hasRead = true;
            }
          });
          if (!hasRead){
            m.read.push(ObjectId(user_id));
            m.markModified('read');
            m.save(function(err, obj){});
          }
        });
      }
      utils.prismResponse(res, messages, true);
    }
  });
};

exports.fetchMessages = function(req, res){
  var group = req.params.group_name;
  var org_id = req.params.org_id;
  var user_id = req.query.requestor;
  var action = req.query.action;
  group = group == 'all'?null:group;
  var criteria = {organization: org_id, group: group, target: null};
  var sort = {create_date: -1};
  if (action) {
    if (action == 'unread') {
      var response = {};
      response._id = group?group:org_id;
      Message.find(criteria)
      .select({_id: 1, read: 1})
      .exec(function(err, c){
        if (err) {
          res.status(500).send(err);
        }
        var unread = _.reject(c, function(obj){
          var read = false;
          _.each(obj.read, function(r, i, l){
            if (String(r) == String(user_id)) {
              read = true;
            }
          });
          return read;
        });
        response.unread_count = unread.length;
        utils.prismResponse(res, response, true);
      });
    }
  } else {
    if (req.query.since) {
      criteria.create_date = {$gt: req.query.since};
      sort = {create_date: 1}
    }
    if (req.query.before) {
      criteria.create_date = {$lt: req.query.before};
      sort = {create_date: -1}
    }
    if (req.query.updated) {
      criteria.modify_date = {$gt: req.query.updated};
    }
    Message.find(criteria)
    .sort(sort)
    .limit(20)
    .exec(function(err, messages){
      if (err) {
        console.log(err);
        res.status(500).send(err);
      } else {
        if (!req.query.since) {
          messages = messages.reverse();
        }
        console.log('user id for request ' + user_id);
        if (user_id){
          _.each(messages, function(m, i, l){
            var hasRead = false;
            _.each(m.read, function(r, id, l){
              if (String(r) == String(user_id)){
                hasRead = true;
              }
            });
            if (!hasRead){
              m.read.push(ObjectId(user_id));
              m.markModified('read');
              m.save(function(err, obj){});
            }
          });
        }
        utils.prismResponse(res, messages, true);
      }
    });
  }
};

exports.updateMessage = function(req, res){
  var action = req.body.action;
  var uid = req.body.user;
  var messageID = req.params.message_id;
  if (action == 'like' || action == 'unlike'){
    if (messageID && uid) {
      Message.findOne({_id: messageID}, function(err, message){
        if (message) {
          if (action == 'like') {
            message.likes.push(uid);
            message.likes_count += 1;
            message.save(function(err, res){
              if (err) console.log(err);
              new Activity({
                from: uid,
                to: message.creator,
                action: 'like',
                message_id: message._id
              }).save(function(err, result){
                if (err) console.log(err);
              });
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
  } else {
   var id = req.params.message_id;
    var text = req.body.text;
    Message.findOne({_id: id})
    .exec(function(err, message){
      if (err) console.log(err);
      if (message) {
      message.text = text;
      message.save(function(err, result){
        if (err) console.log(err);
        else utils.prismResponse(res, result, true);
      });
      } else {
        res.status(400).send();
      }
    });

  } 

}

var fetchTotalMessageCount = function(user_id, org_id, next){
  var countMessages = function(criteria, next){
    Message.find(criteria)
      .select({_id: 1, read: 1})
      .exec(function(err, messages){
        if (messages) {
        var unread = _.reject(messages, function(obj){
          var read = false;
          _.each(obj.read, function(r, i, l){
            if (String(r) == String(user_id)) {
              read = true;
            }
          });
          return read;
        });
        next(unread.length);
        } else {
          next(0);
        }
      });
  };
  User.findOne({_id: user_id})
  .populate({path: 'org_status.groups', model: 'Group'})
  .exec(function(err, user){
    if (user) {
      if (user.type = 'institution_verified') {
        Group.find({organization: org_id, status: {$ne: 'inactive'}}, function(err, groups){
          var groupList = _.pluck(groups, '_id');
          var criteria = {organization: org_id, group: {$in: groupList}};
          countMessages(criteria, next);
        });
      } else {
        var groups = [];
        _.each(user.org_status, function(o,i,l){
          if (String(o.organization) == String(org_id) && o.status == 'active') {
            groups = o.groups;
          }
        });
        groups = _.filter(groups, function(g){
          return g.status != 'inactive';
        });
        groups = _.pluck(groups, '_id');
        var criteria1 = {organization: org_id, group: null};
        var criteria2 = {organization: org_id, group: {$in: groups}};
        var criteria = {$or: [criteria1, criteria2]};
        countMessages(criteria, next);
      }
    } else {
      next(0);
    } 
  });
};

var sendMessageWithMutes = function(user, message, mutes ){
  var send = String(user.id) != String(message.creator._id);
  
  if (send) {
    _.each(mutes, function(m, i, l){
      if (String(m) == String(user._id)){
        send = false;
      }
    });
  }
  if (send){
    fetchTotalMessageCount(user._id, message.organization, function(c){
      console.log('sending');
      Push.sendMessageToUser(message, user, c);
    }); 
  } else {
    console.log('not sending to ' + user.name);
  }
}

var notifyUsers = function(m){
  Message.findOne({_id: m._id})
  .populate({path: 'creator'})
  .populate({path: 'organization', select: '_id name owner mutes'})
  .populate({path: 'organization.owner', select: '_id name'})
  .populate({path: 'group'})
  .populate({path: 'creator', select: {
    id: 1,
    name: 1,
    org_status: {$elemMatch: {
      organization: m.organization,
      status: active
    }}
  }
  })
  .exec(function(err, message){
    var role = false;
    if (message.creator.org_status.length > 0) {
      role = message.creator.org_status[0].role;
    }
    var organization = message.organization;
    if (organization) {
      var criteriaa = {_id: organization.owner._id};
      var criteriab = {};  
      if (message.group){
        criteriab.org_status = {$elemMatch: {status: 'active', organization: organization._id,  groups: {$elemMatch: {$eq: message.group}}}};
      } else {
        criteriab.org_status = {$elemMatch: {organization: organization._id, status: 'active'}};
      }
      var criteria = {$or: [criteriaa, criteriab]};
      if (message.target) {
        criteria = {_id: criteria.target};
      }
      User.find(criteria)
        .populate({path: 'org_status.organization', model: 'Organization'})
        .populate({path: 'org_status.organization.groups', model: 'Group'})
        .exec(function(err, users){
          _.each(users, function(user, i, l){
              var send = true;
              var mutes = message.group?message.group.mutes:organization.mutes;
              if (String(message.creator._id) == String(organization.owner._id) || 
                role == 'leader' || role == 'ambassador' ) {
                mutes = [];
              }
              if (message.target) {
                mutes = [];
              }
              sendMessageWithMutes(user, message, mutes);
          });
      });
  }
  });
}


exports.createMessage = function(req, res){
  var creator = req.body.creator;
  var text = req.body.text;
  var urls = false;
  var group = req.body.group=='all'?null:req.body.group;
  var organization = req.body.organization;
  var imageURL = req.body.image_url;
  if (text) urls = utils.urls(text)
  var message = new Message({
    creator: creator, 
    text: text, 
    group: group, 
    organization: organization,
    image_url: imageURL
  });
  var save = function(){
    message.save(function(err, result){
      if (err) {
        res.status(500).send(err);
      } else {
        notifyUsers(result);
        utils.prismResponse(res, result, true);

      }
    });
  }
  if (urls) {
      request({
        uri: urls[0],
        method: 'GET',
        timeout: 10000,
        followRedirect: true,
        maxRedirects: 10
      }, function(error, response, body) {
        console.log(error);
        var handler = new htmlparser.DefaultHandler(function(err, dom){
          if (err) console.log(err);
          var meta = [];
          var traverse = function(doc) {
            _.each(doc, function(node, i, l){
              if (node.name == 'meta') {
                if (node.attribs.property && node.attribs.property.match('og:')) {
                  meta.push(node.attribs);
                } 
                if (node.attribs.name && node.attribs.name.match('og:')) {
                  meta.push(node.attribs);
                } 
              } 
              if (node.children && node.children.length > 0){
                traverse(node.children);
              }
            });
          };
          traverse(dom);
          if (meta.length > 0){
            var metaData = {image:{}};
            _.each(meta, function(m, i, l){
              var accessor = '';
              if (m.property) accessor = 'property';
              if (m.name) accessor = 'name';
              if (m[accessor] == 'og:image'){
                metaData.image.url = m.content;
              } 
              if (m[accessor] == 'og:image:width') {
                metaData.image.width = m.content;
              }
              if (m[accessor] == 'og:image:height') {
                metaData.image.height = m.content;
              }
              if (m[accessor] == 'og:description'){
                metaData.description = m.content;
              }
              if (m[accessor] == 'og:title'){
                metaData.title = m.content;
              }
              if (m[accessor] == 'og:url'){
                metaData.url = m.content;
              }
              if (m[accessor] == 'og:video:url'){
                if (!metaData.video_url) {
                  metaData.video_url = m.content;
                }
              }
            });
            message.meta = metaData;
          }
          save(message);
        });
        var parser = new htmlparser.Parser(handler);
        parser.parseComplete(body);
      });
    } else {
      save(message);
    }
}

exports.createDirectMessage = function(req, res) {
  var creator = req.body.creator;
  var text = req.body.text;
  var urls = false;
  var target = req.params.uid;
  var organization = req.params.org_id;
  var imageURL = req.body.image_url;
  if (text) urls = utils.urls(text)
  var message = new Message({
    creator: creator, 
    text: text, 
    target: target, 
    organization: organization,
    image_url: imageURL
  });
  var save = function(){
    message.save(function(err, result){
      if (err) {
        res.status(500).send(err);
      } else {
        notifyUsers(result);
        utils.prismResponse(res, result, true);

      }
    });
  }
  if (urls) {
      request({
        uri: urls[0],
        method: 'GET',
        timeout: 10000,
        followRedirect: true,
        maxRedirects: 10
      }, function(error, response, body) {
        console.log(error);
        var handler = new htmlparser.DefaultHandler(function(err, dom){
          if (err) console.log(err);
          var meta = [];
          var traverse = function(doc) {
            _.each(doc, function(node, i, l){
              if (node.name == 'meta') {
                if (node.attribs.property && node.attribs.property.match('og:')) {
                  meta.push(node.attribs);
                } 
                if (node.attribs.name && node.attribs.name.match('og:')) {
                  meta.push(node.attribs);
                } 
              } 
              if (node.children && node.children.length > 0){
                traverse(node.children);
              }
            });
          };
          traverse(dom);
          if (meta.length > 0){
            var metaData = {image:{}};
            _.each(meta, function(m, i, l){
              var accessor = '';
              if (m.property) accessor = 'property';
              if (m.name) accessor = 'name';
              if (m[accessor] == 'og:image'){
                metaData.image.url = m.content;
              } 
              if (m[accessor] == 'og:image:width') {
                metaData.image.width = m.content;
              }
              if (m[accessor] == 'og:image:height') {
                metaData.image.height = m.content;
              }
              if (m[accessor] == 'og:description'){
                metaData.description = m.content;
              }
              if (m[accessor] == 'og:title'){
                metaData.title = m.content;
              }
              if (m[accessor] == 'og:url'){
                metaData.url = m.content;
              }
              if (m[accessor] == 'og:video:url'){
                if (!metaData.video_url) {
                  metaData.video_url = m.content;
                }
              }
            });
            message.meta = metaData;
          }
          save(message);
        });
        var parser = new htmlparser.Parser(handler);
        parser.parseComplete(body);
      });
    } else {
      save(message);
    }

}


exports.deleteMessage = function(req, res){
  var id = req.params.message_id;
  Message.findOne({_id: id})
    .remove(function(err){
      if (err){
        res.status(500).send(err);
      } else {
        res.status(200).send();
      }
    });
}

exports.createGroup = function(req, res){
  var org_id = req.params.org_id;
  var leader = req.body.leader;
  var description = req.body.description;
  var members = req.body.members;
  var name = req.body.name;
  var creator = req.body.creator;
  Organization.findOne({_id: org_id}, function(err, org){
    if (org){
      if (!members) {
        members = [];
      } if (!_.isArray(members)){
        members = [members];
      }
      if (leader) { 
        members.push(leader);
        leader = ObjectId(leader);
      }
      User.find({_id: {$in: members}})
      .exec(function(err, users){
        var group = new Group({
          name: name,
          description: description,
          organization: org._id
        });
        if (users){
          var ids = _.pluck(users, '_id');
          group.members = ids;
        }
        group.leader = leader;
        group.save(function(err, group){
          if (err) {
            res.status(500).send(err);
          } else { 
            org.groups.push(group._id);
            org.save(function(err, r){
              if (err) console.log(err);
            });
            _.each(users, function(u, i, l){
              var idx = -1;
              _.each(u.org_status, function(o, iu, l){
               
                if (String(o.organization) == String(org._id) && o.status == 'active'){
                  idx = iu;                
                }
              });
              if (idx != -1) {
                u.org_status[idx].groups.push(group._id); 
              }
              u.markModified('org_status');
              u.save(function(err, result){
                if (err) console.log(err);
                else {
                  new Activity({
                    action: 'group_added',
                    to: result._id,
                    from: creator,
                    group_id: group._id
                  }).save(function(err, activity){
                    if (err) console.log(err);
                    console.log('finished making activity');
                  });
                }
              });
            });

            utils.prismResponse(res, group, true);
          }
        }); 
             });
    } else {
      res.status(400).send(err);
    }
  });
}

exports.updateGroup = function(req, res){
  var org_id = req.params.org_id;
  var group_id = req.params.group_id;
  var name = req.body.name;
  var description = req.body.description;
  var leader = req.body.leader;
  var action = req.body.action;
  var requestor = req.body.requestor;
  console.log(requestor);
  Group.findOne({_id: group_id}, function(err, group){
    if (group) {
      if (action) {
        if (action == 'mute'){
          var exists = false;
          _.each(group.mutes, function(id, i, l){
            if (String(id) == String(requestor)){
              exists = true;
            }
          });
          if (!exists){
            group.mutes.push(ObjectId(requestor));
            group.markModified('mutes');
            group.save(function(err, obj){});
          }
        } else if (action == 'unmute'){
          var idx = -1;
          _.each(group.mutes, function(id, i, l){
            if (String(id) == String(requestor)){
              idx = i;
            }
          });
          if (idx != -1) {
            console.log('found');
            group.mutes.splice(idx, 1);
            group.markModified('mutes');
            group.save(function(err, obj){});
          }
        }
        utils.prismResponse(res, group, true); 
      } else {
        var fields = {
          name: name,
          description: description
        };
        if (leader) {
          fields.leader = ObjectId(leader);
        }
        group.update(fields, function(err, result){
          if (err) {
            console.log(err);
            res.status(500).send(err);
          } else {
            utils.prismResponse(res, group, true);
          }
        });
      }
      } else {
        res.status(400).send();
      }
  });
};

exports.deleteGroup = function(req, res) {
  var org_id = req.params.org_id;
  var group_id = req.params.group_id;
  var requestor = req.body.requestor;
  User.findOne({_id: requestor}, function(err, user){
    if (!user) {
      res.status(400).send();
      return;
    }
    if (user.type != 'institution_verified') {
      var os = _.find(user.org_status, function(o){
        return String(o.organization) == String(org_id) && o.role == 'leader';
      });
      console.log(os);
      if (!os) {
        res.status(401).send();
        return;
      }
    }
    Organization.findOne({_id: org_id}, function(err, org){
      if (! org) {
        res.status(400).send();
        return;
      }
      if (user.type == 'institution_verified') {
        if (String(org.owner) != String(user._id)) {
          res.status(401).send();
          return;
        }
      }
      Group.findOne({_id: group_id}, function(err, group){
        if (!group || String(group.organization) != String(org_id)) {
          res.status(401).send();
          return;
        }
        group.status = 'inactive';
        group.markModified('status');
        group.save(function (err) {
          if (err) {
            res.status(500).send(err);
          } else {
            res.status(200).send();
          }
        });
      });
    });
  });
};

exports.updateGroupMembers = function(req, res){
  var org_id = req.params.org_id;
  var group_id = req.params.group_id;
  var requestor = req.body.requestor;
  var members = req.body.members;
  console.log(members);
  User.find({org_status: {$elemMatch: {groups: {$elemMatch: {$eq: ObjectId(group_id)}}}}})
  .exec(function(err, users){
    var $users = users?users:[];
    _.each($users, function(u, i, l){
      if (_.indexOf(members, String(u._id)) == -1){
        _.each(u.org_status, function(o, c, d){
          var idx = -1;
          if (String(o.organization) == String(org_id)){
            _.each(o.groups, function(g, m, a){
              if (String(g) == String(group_id)){
                idx = m;
              }
            });
            console.log(u.name + idx);
            if (idx != -1) {
              o.groups.splice(idx, 1);
            }
          }
        });
     }
     var idx = _.indexOf(members, String(u._id));
     console.log('idx: ', idx);
     if (idx != -1) {
      members.splice(idx, 1);
     }
     u.markModified('org_status');
      u.save(function(err, res){
        if (err) console.log(err);
      });
    }); 
    User.find({_id: {$in: members}}, function(err, newusers){
      _.each(newusers, function(u, i, l) {
        _.each(u.org_status, function(o, c, d){
          if (String(o.organization) == String(org_id)) {
            if (!o.groups) o.groups = [];
            var x = -1;
            _.each(o.groups, function(g, index){
              if (String(g) == String(group_id)) {
                x = index;
              }
            });
            if (x == -1) {
              o.groups.push(ObjectId(group_id));
            }
          }
        });
        u.markModified('org_status');
        u.save(function(err, user){
          if (err) console.log(err);
          else {
            new Activity({
              action: 'group_added',
              to: u._id,
              from: requestor,
              group_id: group_id
            }).save(function(err, activity){
              if (err) console.log(err);
              console.log('finished making activity');
            });

          }
        });
        users.push(u);
      });  
      utils.prismResponse(res, users, true);
    }); 
  });
}

exports.deleteUserFromGroup = function(req, res){
  var org_id = req.params.org_id;
  var group_id = req.params.group_id;
  var user_id = req.params.user_id;
  User.findOne({_id: user_id}, function(err, user){
    if (user) {
      _.each(user.org_status, function(o, i, l) {
        if (String(o.organization) == String(org_id)) {
          var idx = -1;
          _.each(o.groups, function(g, p, d){
            if (String(g) == String(group_id)){
              idx = p;
            }
          });
          if (idx != -1) {
            o.groups.splice(idx, 1);
          }
        }
      });
      user.markModified('org_status');
      user.save(function(err, result){
        utils.prismResponse(res, result, true);
      });
    } else {
      res.status(400).send();
    }
  });
};

exports.fetchDirectDigest = function(req, res) {
  var uid = req.params.uid;
  Message.find({target: uid, creator: {$ne: uid}, read: {$ne: ObjectId(uid)}})
  .populate({path: 'creator', model: 'User', select: {_id: 1, name: 1, first_name: 1, last_name: 1, profile_photo_url: 1, active: 1}})
  .exec(function(err, messages){
    console.log(messages);
    utils.prismResponse(res, messages, true);
  });
};
