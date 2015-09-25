var express = require('express');
var app = express();
var mongoose = require('mongoose');
var ObjectId = mongoose.Types.ObjectId;
var config = require('config');
var gateway = require('../gateway');
var _ = require('underscore');

var User = mongoose.model('User');

app.get('/', function(req, res){
  var searchText = req.query.search || false;
  var organization = req.query.organization || false;
  var group = req.query.group || false;
  var limit = req.params.limit || 5;
  var params = {};
  if (searchText) {
    var r = new RegExp(searchText, 'i');
    params.name = r;
  }
  if (organization) {
    var orgStatus = {organization: ObjectId(organization), status: 'active'};
    if (group) {
      orgStatus.group = ObjectId(group);
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

app.get('/:uid', function(req, res){
  var uid = req.params.uid;
  User.findOneCore(uid, function(err, user){
    if (user) {
      res.status(200).json(user);
    } else {
      res.status(400).json(err);
    }
  });
});

module.exports = app;
