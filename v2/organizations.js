var express = require('express');
var app = express();
var mongoose = require('mongoose');
var config = require('config');
var gateway = require('../gateway');
var Organization = mongoose.model('Organization');
var User = mongoose.model('User');
var Message = mongoose.model('Message');

// Organization Endpoints

app.get('/', function(req, res){
  Organization.find({}, function(err, orgs){
    res.status(200).json(orgs);
  });
});

app.get('/:org_id/groups', function(req, res) {
  console.log('in request');
  var org_id = req.params.org_id;
  if (org_id) {
    Organization.findOne({_id: org_id}, function(err, org){
      if (org) {
        org.fetchGroups(function(err, groups){
          if (err) {
            console.log(err);
            res.status(500).json(err);
          } else {
            res.status(200).json(groups);
          }
        });
      } else {
        console.log(err);
        res.status(400).send();
      }
    });
    
  } else {
    res.status(400).send();
  }
});

app.get('/:org_id/users/:uid/groups', function(req, res) {
  var uid = req.params.uid;
  var org_id = req.params.org_id;
  if (uid && org_id) {
    User.findOne({_id: uid}, function(err, user) {
      if (user) {
        user.fetchGroups(org_id, function(err, groups){
          if (err) {
            console.log(err);
            res.status(500).json(err);
          } else {
            res.status(200).json(groups);
          }
        });
      } else {
        res.status(400).send();
      }
    });
  } else {
    res.status(400).send();
  }
});

app.get('/:org_id/groups/:gid/messages', function(req, res) {
  var org_id = req.params.org_id;
  var gid = req.params.gid != "all"?req.params.gid:false;
  var before = req.query.before?new Date(req.query.before):false;
  var after = req.query.after?new Date(req.query.after):false;
  var limit = req.query.limit || false;
  var requestor = req.query.requestor || false;
  var params = {organization: org_id, target: null};
  if (gid) {
    params.group = gid;
  } else {
    params.group = null;
  }
  var createDateBefore = false;
  var createDateAfter = false;
  if (after) {
    createDateAfter = {$gt: after};
  }
  if (before) {
    createDateBefore = {$lt: before};
  }
  var createDateRequest = false;
  if (before) {
    if (after) {
      createDateRequest = {$and: [createDateBefore, createDateAfter]}; 
    } else {
      createDateRequest = createDateBefore;
    }
  } else if (after) {
    createDateRequest = createDateAfter;
  }

  if (createDateRequest) {
    params.create_date = createDateRequest;
  }

  Message.findAndFlatten(params, limit, function(err, messages){
    if (err) {
      res.status(500).json(err);
    } else {
      messages = Message.isLiked(messages, requestor); 
      res.status(200).json(messages);
    }
  });
});

app.post("/:oid/groups/:gid/messages", function(req, res){
  var oid = req.params.oid;
  var gid = req.params.gid;
  if (gid == "all") {
    gid = null;
  }
  var text = req.body.text;
  var creator = req.body.creator;
  Message.createMessage({
    organization: oid,
    group: gid,
    text: text,
    creator: creator
  }, function(err, message){
    if (err) {
      res.status(500).json(err);
    } else {
      res.status(200).json([message]);
    }
  });
})

app.post("/:oid/groups/:gid/messages/:mid", function(req, res){
  var mid = req.params.mid;
  var requestor = req.body.requestor;
  Message.likeMessage(mid, requestor, function(err, message){
    if (err) res.status(500).json(err);
    else {
      res.status(200).json(message);
    }
  });
});

module.exports = app;
