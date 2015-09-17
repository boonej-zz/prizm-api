var express = require('express');
var app = express();
var mongoose = require('mongoose');
var config = require('config');
var gateway = require('../gateway');
var Organization = mongoose.model('Organization');
var User = mongoose.model('User');

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

module.exports = app;
