var express = require('express');
var app = express();
var mongoose = require('mongoose');
var gateway = require('../gateway');
var image = require('../lib/helpers/image');
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
 * @apiDefine SinglePostSuccess
 * @apiSuccess {String} _id Unique identifier for post
 * @apiSuccess {String=achievement,aspiration,experience,inspiration,passion,personal} category Category for post
 * @apiSuccess {Date} create_date Date post was created
 * @apiSuccess {String} external_link Link to externally referenced page
 * @apiSuccess {String} external_provider Host of external content
 * @apiSuccess {String} file_path Path to Prizm image content
 * @apiSuccess {String} hash_tags Concatenated list of hash tags
 * @apiSuccess {Boolean} is_flagged Is the post flagged inappropriate?
 * @apiSuccess {Boolean} is_repost Has the post been reposted?
 * @apiSuccess {Number} likes_count Number of likes for post
 * @apiSuccess {Boolean} is_liked Does the requesting user like the post?
 * @apiSuccess {Date} modify_date Date the post was last modified
 * @apiSuccess {String} origin_post_id Original post if the post was reposted
 * @apiSuccess {String} text Text to print in post comments
 * @apiSuccess {String} creator_id Unique identifier of creator
 * @apiSuccess {String=user,institution,institution_verified} creator_type User type of creator
 * @apiSuccess {String} creator_subtype Subtype of creator
 * @apiSuccess {String} creator_profile_url Path to creator's avatar
 * @apiSuccess {String} creator_name Friendly name of creator
 * @apiSuccessExample Single Post:
 *  HTTP/1.1 200 OK
 *  {
 *    "_id": "537a5b3cea72039701295c3e",
 *    "category": "inspiration",
 *    "comments_count": 1,
 *    "create_date": "537a5b3cea72039701295c3e",
 *    "external_link": "http://instagram.com/p/oMKuEwy3tE/",
 *    "external_provider": "instagram",
 *    "file_path": "https:/s3.amazonaws.com/higheraltitude.prism/536aaa3cba551f8541b99ee8/20140519032746_5CDF5DB4-D5CC-4C49-8513-23B91C06AA92.jpg",
 *    "hash_tags": "#beprizmatic #prizm",
 *    "is_flagged": false,
 *    "is_repost": false,
 *    "likes_count": 1,
 *    "is_liked": false,
 *    "modify_date": "https:/s3.amazonaws.com/higheraltitude.prism/536aaa3cba551f8541b99ee8/20140519032746_5CDF5DB4-D5CC-4C49-8513-23B91C06AA92.jpg"
 *    "origin_post_id": null,
 *    "text": "#prizm #beprizmatic",
 *    "creator_id": "536aaa3cba551f8541b99ee8",
 *    "creator_type": "user",
 *    "creator_subtype: "luminary",
 *    "creator_profile_photo_url": "https://somepath.com/image.jpg",
 *    "creator_name": "John Smith"
 *  } 
 **/

/**
 * @api {get} / Get Posts
 * @apiName GetPosts
 * @apiGroup Posts
 * @apiDescription Returns an array of posts from all users based on the 
 *  requestor's trusts, group membership, and other parameters passed. 
 *  This is the basis for the Explore feed and logic.
 * @apiParam (Query) {String} requestor Unique id for requesting user
 * @apiParam (Query) {String} [hashtag] Hashtag to filter based on
 * @apiParam (Query) {String=featured,popular} [mode] Use a tabbed search mode
 * @apiParam (Query) {Date} [before] Search for posts before a given date
 * @apiParam (Query) {Date} [after] Search for posts after a given date
 * @apiParam (Query) {Number} [limit] Limit the return to the number of posts
 * @apiUse Error
 **/
app.get('/', function(req, res) {
  var requestor = req.query.requestor;
  var hashtag = req.query.hashtag || false;
  var mode = req.query.mode || false;
  var before = req.query.before || false;
  var after = req.query.after || false;
  var limit = req.query.limit || false;
  if (!requestor) {
    Error.invalidRequest(res, 'You must include a requestor with this request.');
  } else {
    Post.fetchExplore({
      requestor: requestor,
      hashtag: hashtag,
      mode: mode,
      before: before,
      after: after,
      limit: limit
    }, function(err, posts) {
      if (err) Error.serverError(res);
      else res.status(200).json(posts);
    });
  }
});

/**
 * @api {get} /posts/:pid Get Post
 * @apiName GetPost
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam (Query) {String} [requestor] Unique Id for requesting user
 * @apiUse SinglePostSuccess
 * @apiUse Error
 **/
app.get('/:pid', function(req, res){
  var pid = req.params.pid;
  var uid = req.query.requestor;
  if (pid && uid) {
    Post.fetchPost(pid, uid, function(err, post){
      if (err) {
        Error.serverError(res);
      } else {
        res.status(200).json(post);
      }
    });
  } else {
    Error.invalidRequest(res, 'You must specify a user id and post id.');
  }
});

/**
 * @api {get} /posts/:pid/likes Get Post Likes
 * @apiName GetPostLikes
 * @apiGroup Posts
 *
 * @apiParam {String} pid Unique ID for post
 * @apiUse UserShortSuccess
 * @apiUse Error
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
 * @apiUse Error
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
 * @apiUse Error
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
 * @api {post} /posts/:pid/comments Create Comment
 * @apiName CreatePostComments
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam (Body) {String} creator Unique ID for comment creator
 * @apiParam (Body) {String} text Comment text
 * @apiUse CommentSuccess
 * @apiUse Error
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
 * @api {get} /posts/:pid/comments Get Comments
 * @apiName GetPostComments
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiUse CommentSuccess
 * @apiUse Error
 **/
app.get('/:pid/comments', function(req, res) {
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
 * @api {get} /posts/:pid/comments/:cid/likes Get Comment Likes
 * @apiName GetPostCommentLikes
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} cid Unique ID for coment
 * @apiUse UserShortSuccess
 * @apiUse Error
 **/
app.get('/:pid/comments/:cid/likes', function(req, res) {
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
 * @api {put} /posts/:pid/comments/:cid/likes Like Comment
 * @apiName AddPostCommentLike
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} cid Unique ID for comment
 * @apiParam (Body) user Unique ID for user
 * @apiUse UserShortSuccess
 * @apiUse Error
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
 * @api {delete} /posts/:pid/comments/:cid/likes/:uid Unlike Comment
 * @apiName DeletePostCommentLike
 * @apiGroup Posts
 * @apiParam {String} pid Unique ID for post
 * @apiParam {String} cid Unique ID for comment
 * @apiParam {String} uid Unique ID for user
 * @apiUse UserShortSuccess
 * @apiUse Error
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

// CREATE
/**
 * @api {post} / Create Post
 * @apiName CreatePost
 * @apiGroup Posts
 * @apiParam (Body) {String=aspiration,inspiration,passion,experience, achievement,personal} category Category for post
 * @apiParam (Body) {String=public,trust,private} scope Visibility scope for 
 *  post
 * @apiParam (Body) {String} creator Unique id for post's creator
 * @apiParam (Body) {String} [file_path] Path to image representing post
 * @apiParam (Body) {String} [text] Text content for post
 * @apiParam (Body) {Number} [location_latitude] Latitude associated with post
 * @apiParam (Body) {Number} [location_longitude] Longitude associated with post
 * @apiParam (Body) {Array} [hash_tags] A JSON array of hash_tags
 * @apiUse Error
 **/
app.post('/', function(req, res) {
  var creator = req.body.creator;
  var scope = req.body.scope;
  var category = req.body.category;
  var text = req.body.text;
  var file_path = req.body.file_path;
  var location_latitude = req.body.location_latitude;
  var location_longitude = req.body.location_longitude;
  var hash_tags = req.body.hash_tags || [];
  var origin_post_id = req.body.origin_post_id || false;

  if (creator && scope && category) {
    var post = new Post({creator: creator, scope: scope, category: category, 
      text: text, file_path: file_path, location_latitude: location_latitude,
      location_longitude: location_longitude, hash_tags: hash_tags, 
      hash_tags_count: hash_tags.length});
    if (origin_post_id) {
      post.origin_post_id = origin_post_id;
      post.is_repost = true;
    }
    if (!file_path) {
      var at = post.text;
      User.resolvePostTags(post, function(err, users) {
        var match = String(post.text).match(/@\S{24}/g);
        if (match && match.length > 0) {
          _.each(match, function(m){
            var uid = m.substr(1);
            var mu = _.find(users, function(u){
              return (String(u._id) == String(uid));
            });
            if (mu) {
              at = at.replace(m, '@' + mu.name);
            }
          });
          
        }
        image.saveTextImage(post, at, function(err, path){
            post.file_path = path;
            post.save(function(err, result){
              if (err) {
                Error.serverError(res);
              } else {
                res.status(200).json(result);
              }
            });
          }); 
        
      });
    } else {
      post.save(function(err, result){
        if (err) {
          Error.serverError(res);
        } else {
          res.status(200).json(result);
        }
      });
    }
    
  } else {
    Error.invalidRequest(res, 'You must include a creator, scope, and category.');
  }
});

/**
 * @api {put} /posts/:pid Update Post
 * @apiName ModifyPost
 * @apiDesc Modifies a posts contents.
 * @apiGroup Posts
 * @apiVersion 2.0.0
 * @apiParam {String} pid Unique identifier for post
 * @apiParam (Body) {String=achievement,aspiration,experience,inspiration,passion,personal} [category] Category of post
 * @apiParam (Body) {String=public,private,trust} [scope] Scope of post
 * @apiParam (Body) {String} [text] Text to display
 **/
app.put('/:pid', gateway, function(req, res) {
  var pid = req.params.pid;
  var category = req.body.category || null;
  var scope = req.body.scope || null;
  var text = req.body.text || null;
  if (pid) {
    Post.findOne({_id: pid}, function(err, post) {
      if (err) {
        Error.serverError(res);
      } else {
        if (category) {
          post.category = category;
        }
        if (scope) {
          post.scope = scope;
        }
        if (text) {
          post.text = text;
        }
        post.save(function(err, result){
          if (err) {
            Error.ServerError(res);
          } else {
            res.status(200).json(result);
          }
        });
      }
    });
  } else {
    Error.invalidRequest(res, "You must include a post id.");
  }
});

/**
 * @api {delete} /posts/:pid Delete Post
 * @apiName DeletePost
 * @apiDesc Deletes a post.
 * @apiGroup Posts
 * @apiVersion 2.0.0
 * @apiParam {String} pid Unique identifier for post
 **/
app.delete('/:pid', gateway, function(req, res) {
  var pid = req.params.pid;
  if (pid) {
    Post.findOne({_id: pid}, function(err, post) {
      if (post) {
        post.status = 'deleted';
        post.delete_date = Date.now();
        post.save(function(err, result) {
          if (err) {
            Error.serverError(res);
          } else {
            User.findOne({_id: post.creator}, function(err, user){
              user.posts_count = user.posts_count -1;
              user.save(function(err, user_saved){
              });
            });
            res.status(200).send();
        }
        });
      } else {
        Error.serverError(res);
      }
    });
  } else {
    Error.invalidRequest(res, "You must include a post id.");
  }
});

app.put('/:pid/flags', function(req, res){
  var pid = req.params.pid || false;
  var reporter = req.body.reporter || false;
  if (pid && reporter) {
    Post.findOne({_id: pid}, function(err, post){
      if (post) {
        var flagged = false;
        _.each(post.flagged_reporters, function(r){
          if (String(r.reporter_id) == String(pid)) {
            flagged = true;
          }
        });
        
        if (!flagged) {
          post.flagged_reporters.push({reporter_id: pid, create_date: Date.now()});
          post.save(function(err, saved){
            res.status(200).json(saved);
          });
        } else {
          res.status(200).json(post);
        }
      } else {
        Error.invalidRequest('No such post', res);
      }
    }); 
  } else {
    Error.invalidRequest('Must provide a post id and reporter id', res);
  }
});

module.exports = app;
