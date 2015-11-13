var express = require('express');
var app = express();
var mongoose = require('mongoose');
var ObjectId = mongoose.Types.ObjectId;
var config = require('config');
var gateway = require('../gateway');
var _ = require('underscore');
var moment = require('moment');

var User = mongoose.model('User');
var Interest = mongoose.model('Interest');
var Post = mongoose.model('Post');
var Activity = mongoose.model('Activity');
var Trust = mongoose.model('Trust');

app.get('/', gateway, function(req, res){
  var searchText = req.query.search || false;
  var organization = req.query.organization || false;
  var group = req.query.group || false;
  var limit = req.params.limit || false;
  var params = {};
  if (searchText) {
    var r = new RegExp(searchText, 'i');
    params.name = r;
  }
  if (organization) {
    var orgStatus = {organization: ObjectId(organization), status: 'active'};
    if (group) {
      orgStatus.groups = ObjectId(group);
    }
    params.org_status = {$elemMatch: orgStatus};
  }
  console.log(params);
  User.findBasic(params, limit, function(err, users){
    if (err) {
      res.status(500).json(err);
    } else {
      res.status(200).json(users);
    }
  });
});

app.get('/:uid', gateway, function(req, res){
  var uid = req.params.uid;
  uid = uid.replace(" ", "");
  User.findOneCore(uid, function(err, user){
    if (err) console.log(err);
    if (user) {
      res.status(200).json(user);
    } else {
      res.status(400).json(err);
    }
  });
});

// Register Device
app.put('/:uid/devices', gateway, function(req, res){
  var uid = req.params.uid;
  var device = req.body.device;
  User.registerDevice(uid, device, function(err, user){
    if (err){
      res.status(500).json(err);
    } else {
      res.status(200).json(user);
    }
  });
});

// Update User
app.put('/:uid', gateway, function(req, res) {
  var uid = req.params.uid;
  var post = req.body;
  if (post.date_founded) {
    post.date_founded = moment(post.date_founded).toDate() || null;
  } 
  User.findOneAndUpdate({_id: uid}, post, function(err, user){
    if (err) {
      console.log(err);
      res.status(500).json(err);
    } else {
      console.log('user updated');
      User.findOneCore(uid, function(err, user){
        if (err) {
          res.status(500).json(err);
          console.log(err);
        } else res.status(200).json(user);
      });
    }
  });
});

// INTERESTS
app.get('/:uid/interests', gateway, function(req, res){
  var uid = req.params.uid;
  User.findOne({_id: uid}, {_id: 1, interests: 1}, function(err, user){
    if (err) res.status(500).send(err);
    else {
      Interest.find({})
      .exec(function(err, interests){
        var result = [];
        if (user && _.isArray(user.interests)) {
          _.each(interests, function(i){
            i = i.toObject();
            var selected = false;
            _.each(user.interests, function(ui){
              var comp = ui._id?String(ui._id):String(ui);
              if (String(i._id) == String(comp)) {
                selected = true;
              }
            });
            i.selected = selected;
            result.push(i);
          });
        } else {
          _.each(interests, function(i){
            i = i.toObject();
            i.selected = false;
            result.push(i);
          });
        }
        res.status(200).json(result);
      });
    }
  });
});

app.put('/:uid/interests', gateway, function(req, res){
  var uid = req.params.uid;
  var interests = JSON.parse(req.body.interests);
  var newArray = [];
  if (!_.isArray(interests)) interests = [interests];
  _.each(interests, function(i){
    newArray.push(ObjectId(i.uniqueId));
  });
  User.findOne({_id: uid}, function(err, user){
    user.interests = newArray;
    user.save(function(err, user){
      if (err) res.status(500).json(err);
      else res.status(200).json(user);
    });
  });
});

/** Home Feed **/
app.get('/:uid/home', function(req, res) {
  var uid = req.params.uid;
  var last = req.query.last;
  User.fetchHomeFeedCriteria(uid, last, function(err, criteria){
    if (err) res.status(400).json(err);
    Post.fetchHomeFeed(uid, criteria, function(err, posts){
      if (err) res.status(500).json(err);
      else res.status(200).json(posts);
    });
  });
}); 

/** Activity **/
app.get('/:uid/activities', function(req, res) {
  var uid = req.params.uid;
  var last = req.query.last;
  var filter = req.query.filter;
  if (filter == 'counts') {
    var result = {};
    Activity.count({to: uid, has_been_viewed: false}, function(err, c){
      result.activities = c;
      Trust.count({to: uid, status: 'pending'}, function(err, pc){
        result.trusts = pc;
        res.json(result);
      });
    });
  } else {
    Activity.fetchActivitiesForUser(uid, last, function(err, activities){
      if (err)res.json(err);
      else res.json(activities);
    });
  }
});

/** Trusts **/
app.get('/:uid/trusts', function(req, res) {
  var uid = req.params.uid;
  var last = req.query.last;
  var filter = req.query.filter;
  if (filter == 'activity') {
    Trust.fetchTrustActivityForUser(uid, last, function(err, trusts){
      if (err) res.status(400).json(err);
      else res.status(200).json(trusts);
    });
  }
});

module.exports = app;
