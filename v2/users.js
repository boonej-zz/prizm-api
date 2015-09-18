var express = require('express');
var app = express();
var mongoose = require('mongoose');
var config = require('config');
var gateway = require('../gateway');
var _ = require('underscore');

var User = mongoose.model('User');

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
