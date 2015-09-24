var express = require('express');
var app = express();
var mongoose = require('mongoose');
var config = require('config');
var gateway = require('../gateway');
var _ = require('underscore');

var User = mongoose.model('User');

app.get('/', function(req, res){
  var searchText = req.query.search || false;
  var limit = req.params.limit || 5;
  var params = {};
  if (searchText) {
    var r = new RegExp(searchText, 'i');
    params.name = r;
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
