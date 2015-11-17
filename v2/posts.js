var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var Post = mongoose.model('Post');

var User = mongoose.model('User');

/** LIKES - GET **/
app.get('/:pid/likes', function(req, res){
  var pid = req.params.pid;
  Post.findOne({_id: pid})
  .select({likes: 1})
  .populate({path: 'likes._id', model: 'User', select: {_id: 1, name: 1, type: 1, subtype: 1, profile_photo_url: 1}})
  .exec(function(err, post){
    var users = _.pluck(post.likes, '_id');
    res.status(200).json(users);
  });
});

/** LIKES - ADD **/
app.put('/:pid/likes', function(req, res){
  var uid = req.body.user;
  var pid = req.params.pid;
  Post.likePost(pid, uid, function(err, post){
    if (err) res.status(400).json(err);
    else res.status(201).json(post);
  });
});

module.exports = app;
