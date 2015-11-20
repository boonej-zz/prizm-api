var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var _ = require('underscore');
var Post = mongoose.model('Post');
var Error = require('../lib/error');

var User = mongoose.model('User');
/**
 * @apiDefine CommentSuccess
 * @apiSuccess {String} _id Unique ID of comment
 * @apiSuccess {String} status Status of comment
 * @apiSuccess {Number} likes_count Number of likes
 * @apiSuccess {Date} create_date Date comment created
 * @apiSuccess {String} time_since User friendly date string
 * @apiSuccess {String} creator_id Unique ID of comment creator
 * @apiSuccess {String} creator_profile_photo_url Creator's avatar link
 * @apiSuccess {String} creator_type Creator's user type
 * @apiSuccess {String} creator_subtype Creator's subtype
 * @apiSuccess {Boolean} comment_liked True if requestor liked the comment
 * @apiSuccess {Boolean} own_comment True if requestor created the comment
 **/

/**
 * @api {get} /posts/:pid/likes Get Post Likes
 * @apiName GetPostLikes
 * @apiGroup Posts
 *
 * @apiParam {String} pid Unique ID for post
 * @apiParam (Query) {String} requestor Unique ID for requestor
 *
 * @apiUse UserShortSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.get('/:pid/likes', gateway, function(req, res){
  var pid = req.params.pid;
  if (pid) {
    Post.findOne({_id: pid})
    .select({likes: 1})
    .populate({path: 'likes._id', model: 'User', select: {_id: 1, name: 1, type: 1, subtype: 1, profile_photo_url: 1}})
    .exec(function(err, post){
      if (err) Error.serverError(res);
      else {
        var users = _.pluck(post.likes, '_id');
        res.status(200).json(users);
      }
    });
  } else {
    Error.invalidRequest(res, 'You must provide a post id.');
  }
});

/**
 * @api {put} /posts/:pid/likes Like Post
 * @apiName PutPostLikes
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam (Body) {String} user Unique ID for user
 * @apiUse UserShortSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.put('/:pid/likes', gateway, function(req, res){
  var uid = req.body.user;
  var pid = req.params.pid;
  if (uid && pid) {
    Post.likePost(pid, uid, function(err, post){
      if (err) Error.serverError(res);
      else res.status(201).json(post);
    });
  } else {
    Error.invalidRequest(res, 'You must provide a user and post id.');
  }
});

/**
 * @api {delete} /posts/:pid/likes/:uid Unlike Post
 * @apiName DeletePostLikes
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} uid Unique ID for user
 * @apiUse UserShortSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.delete('/:pid/likes/:uid', gateway, function(req, res){
  var uid = req.params.uid;
  var pid = req.params.pid; 
  if (uid && pid) {
    Post.unlikePost(pid, uid, function(err, post){
      if (err) res.status(400).json(err);
      else res.status(200).json(post);
    });
  } else {
    Error.invalidRequest(res, 'You must provide a user and post id.');
  }
});

/** COMMENTS**/
// CREATE
/**
 * @api {post} /posts/:pid/comments Create Post Comment
 * @apiName CreatePostComments
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam (Body) {String} creator Unique ID for comment creator
 * @apiParam (Body) {String} text Comment text
 * @apiUse CommentSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
**/
app.post('/:pid/comments', gateway, function(req, res) {
  var pid = req.params.pid;
  var text = req.body.text;
  var uid = req.body.creator;
  
  if (pid && uid && text) { 
  Post.createComment(pid, uid, text, function(err, comments){
    if (err) Error.serverError(res);
    else res.status(200).json(comments);
  });
  } else {
    Error.invalidRequest(res, "Must provide post id, user id, and comment text.");
  }
});

// READ
/**
 * @api {get} /posts/:pid/comments Get Post Comments
 * @apiName GetPostComments
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiUse CommentSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.get('/:pid/comments', gateway, function(req, res) {
  var pid = req.params.pid;
  var uid = req.query.requestor;
  if (!uid || !pid) {
    Error.invalidRequest(res, 'You must provide a user id and a post id.');
    return;
  } else {
    Post.fetchComments(pid, uid, function(err, comments){
      if (err) Error.serverError(res);
      else res.status(200).json(comments);
    });
  }
});

/**
 * @api {get} /posts/:pid/comments/:cid/likes Get Post Comment Likes
 * @apiName GetPostCommentLikes
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} cid Unique ID for coment
 * @apiUse UserShortSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.get('/:pid/comments/:cid/likes', gateway, function(req, res) {
  var pid = req.params.pid;
  var cid = req.params.cid;
  if (pid && cid) {
    Post.fetchCommentLikes(pid, cid, function(err, users) {
      if (err) Error.serverError(res);
      else res.status(200).json(users);
    });
  } else {
    Error.invalidRequest(res, 'You must provide a post and comment id.');
  }
});

// UPDATE
/** LIKE COMMENT **/
/**
 * @api {put} /posts/:pid/comments/:cid/likes Add Post Comment Like
 * @apiName AddPostCommentLike
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} cid Unique ID for comment
 * @apiParam (Body) user Unique ID for user
 * @apiUse UserShortSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.put('/:pid/comments/:cid/likes', gateway, function(req, res) {
  var uid = req.body.user;
  var pid = req.params.pid;
  var cid = req.params.cid;
  if (uid && pid && cid) {
    Post.likeComment(pid, cid, uid, function(err, comment) {
      if (err) Error.serverError(res);
      else res.status(200).json(comment);
    });
  } else {
    Error.invalidRequest(res, 'You must provide a user id, post id, and comment id.');
  }
});

// DELETE
/** UNLIKE COMMENT **/
/**
 * @api {delete} /posts/:pid/comments/:cid/likes/:uid Delete Post Comment Like
 * @apiName DeletePostCommentLike
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} cid Unique ID for comment
 * @apiParam {String} uid Unique ID for user
 * @apiUse UserShortSuccess
 * @apiUse ErrorInvalid
 * @apiUse ErrorServer
 **/
app.delete('/:pid/comments/:cid/likes/:uid', gateway, function(req, res) {
  var uid = req.params.uid;
  var pid = req.params.pid;
  var cid = req.params.cid;
  if (!uid || !pid || !cid) {
    Error.invalidRequest(res, 'You must provide a user id, post id, and comment id'); 
    return;
  }
  Post.unlikeComment(pid, cid, uid, function(err, comment) {
    if (err) Error.serverError(res);
    else res.status(200).json(comment);
  });
});


module.exports = app;
