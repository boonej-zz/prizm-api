var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');

var User = mongoose.model('User');

/** LIKES - ADD **/
app.put('/:pid/likes', function(req, res){
  var uid = req.body.user;
  var pid = req.params.pid;
  post.findPost(pid, uid, function(err, post){
    if (err) res.status(400).json(err);
    else res.status(201).json(post);
  });
});
