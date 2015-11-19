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

/** LIKES -DELETE **/
app.delete('/:pid/likes/:uid', function(req, res){
  var uid = req.params.uid;
  var pid = req.params.pid;
  Post.unlikePost(pid, uid, function(err, post){
    if (err) res.status(400).json(err);
    else res.status(200).json(post);
  });
});

/** FETCH COMMENTS **/
app.get('/:pid/comments', function(req, res) {
  var pid = req.params.pid;
  var uid = req.query.requestor;
  if (!uid) {
    res.status(400).json({error: 'malformed request'});
    return;
  } else {
    Post.fetchComments(pid, uid, function(err, comments){
      if (err) res.status(500).json(err);
      else res.status(200).json(comments);
    });
  }
});


module.exports = app;
