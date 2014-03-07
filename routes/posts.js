/**
 * Handles `Post` routing & management for all endpoints
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _prism_home   = process.env.PRISM_HOME,
    _utils        = require(_prism_home + 'utils'),
    _logger       = require('winston'),
    PrismError    = require(_prism_home + 'error'),
    Facebook      = require(_prism_home + 'classes/Facebook'),
    Twitter       = require(_prism_home + 'classes/Twitter'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post,
    Comment       = require(_prism_home + 'models/post').Comment;

/**
 * Creates a comment object and add it to the comments property array for a
 * specified post
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.createPostComment = function(req, res){
  if(req.params.id && req.body.creator && req.body.text){

    User.findOne({_id: req.body.creator}, function(err, user){
      if(err) _utils.prismResponse(res, null, false, PrismError.invalidRequest);

      // var comment = {_id: req.body.creator, text: req.body.text, date: new Date()};
      var comment = new Comment({
        text: req.body.text,
        creator: req.body.creator,
        create_date: Date.now()
      });

      Post.findOne({_id: req.params.id}, function(err, post){
        if(err || !post){
          _utils.prismResponse(res,null,false,PrismError.invalidRequest);
        }else{
          post.comments.push(comment);
          post.comments_count = post.comments_count + 1;
          post.save(function(err, saved){
            if(err) _utils.prismResponse(res,null,false,PrismError.serverError);
            var comment_creator = {
              name: user.name,
              first_name: user.first_name,
              last_name: user.last_name,
              _id: user._id,
              profile_photo_url: user.profile_photo_url
            };
            var comment_with_user = {
              create_date: comment.create_date,
              text: comment.text,
              creator: comment_creator,
              _id: comment._id
            };

            var response = {comments: comment_with_user, comments_count: saved.comments_count};
            _utils.prismResponse(res, response, true);
          });
        }
      });
    });
  }else{
    _utils.prismResponse(res,null,false,PrismError.invalidRequest);
  }
};

/**
 * [removePostComment description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.removePostComment = function(req, res){
  var before_count;
  var delete_error = {
    status_code: 400,
    error_info: {
      error: 'unable_to_delete_comment',
      error_description: 'The requested comment has already been removed.'
    }
  };

  if(req.params.id && req.params.comment_id){
    Post.findOne({
      _id: req.params.id,
      "comments._id": req.params.comment_id
    })
    .exec(function(error, comment){
      if(error || !comment) _utils.prismResponse(res, null, false, PrismError.invalidRequest);
      before_count = comment.comments_count;
      var deleted = false;
      for(var i = 0; i < comment.comments.length; i++){
        if(comment.comments[i]._id.toString() === req.params.comment_id){
          comment.comments.splice(i, 1);
          comment.comments_count--;
          deleted = true;
        }
      }

      if(deleted){
        comment.save(function(error, saved){
          if(error || saved.comments_count > before_count)
            _utils.prismResponse(res, null, false, PrismError.serverError);
          var response = {comments_count: saved.comments_count, comments:[]};
          _utils.prismResponse(res, response, true);
        });
      }else{
        _utils.prismResponse(res, null, false, delete_error);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * Fetches paged comments for a specified post
 *
 * The return object should also have the creators populated short user object
 * returned as well
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.fetchPostComments = function(req, res){
  if(req.params.id){
    var query, options = {}, criteria;
    options = _utils.parsedQueryOptions(req.query);
    if(req.query.feature_identifer){

      if(req.query.direction && req.query.direction == 'older'){

        criteria = {
          _id: req.params.id,
          "comments.create_date" : { $lt : req.query.feature_identifer }
        };

      }else{
        criteria = {
          _id: req.params.id,
          "comments.create_date" : { $gt : req.query.feature_identifier }
        };
      }
      query = _utils.buildQueryObject(Post, criteria, options);

    }else{
      criteria  = { _id: req.params.id };
      query     = _utils.buildQueryObject(Post, criteria, options);
    }

    query
    // .select('comments')
    .populate('comments.creator', '_id first_name last_name profile_photo_url name')
    .exec(function(err, comm){
      if(err) _utils.prismResponse(res,null,false,PrismError.ServerError);
      // var comments = comm[0].comments;
      // var response = {comments: []};
      // for(var i=0; i < comments.length; i++){
      //   var stripped = {
      //     creator: {_id: null, first_name:null,last_name:null,name:null,profile_photo_url:null},
      //     _id: null,
      //     text: null,
      //     create_date: null
      //   };

      //   stripped.creator._id = comments[i].creator._id;
      //   stripped.creator.first_name = comments[i].creator.first_name;
      //   stripped.creator.last_name = comments[i].creator.last_name;
      //   stripped.creator.name = comments[i].creator.name;
      //   stripped.creator.profile_photo_url = comments[i].creator.profile_photo_url;
      //   stripped.create_date = comments[i].create_date;
      //   stripped._id = comments[i]._id;
      //   stripped.text = comments[i].text;
      //   response.comments.push(stripped);
      // }

      // response._id = comm[0]._id;
      // response.comments_count = comm[0].comments_count;
      _utils.prismResponse(res,comm,true);

    });
  }else{
    _utils.prismResponse(res,null,false,PrismError.invalidRequest);

  }
};

/**
 * Creates a requestor `like` assocation to the Post Object
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.likePost = function(req, res){
  if(req.params.id && req.body.creator){
    Post.findOne({_id: req.params.id}, function(err, post){
      if(err || !post){
        _utils.prismResponse( res,
                              null,
                              false,
                              PrismError.invalidRequest);

      }else{
        var alreadyLikes = false;
        for(var i=0; i < post.likes.length; i++){
          if(post.likes[i]._id == req.body.creator){
            alreadyLikes = true;
          }
        }

        if(alreadyLikes){
          var error = {
            status_code: 400,
            error_info: {
              error: 'already_liked',
              error_description: 'The requestor has already liked this post'
            }
          };
          _utils.prismResponse( res, null, false, error);

        }else{

          post.likes.push({_id: req.body.creator});
          post.likes_count = post.likes_count + 1;
          post.save(function(err, result){
            if(err){
              _utils.prismResponse( res,
                                    null,
                                    false,
                                    PrismError.serverError);
            }else{
              //return successful response with like id & count
              var response = {  likes_count: post.likes_count,
                                likes: [{_id: req.body.creator.toString()}] };
              _utils.prismResponse(res, response, true);
            }
          });
        }
      }
    });
  }else{
    _utils.prismResponse( res,
                          null,
                          false,
                          PrismError.invalidRequest);
  }
};

/**
 * [likeComment description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.likeComment = function(req,res){
  if(req.params.id && req.params.comment_id && req.body.creator){
    Post.findOne({
      _id: req.params.id,
      "comments._id": req.params.comment_id
    })
    .select('comments')
    .exec(function(error, comment){
      if(error || !comment) _utils.prismResponse(res, null, false, PrismError.invalidRequest);

  /********************** THIS NEEDS TO BE OPTIMIZED **************************/

      var already_likes = false;
      var comment_index = null;
      if(comment.comments.length > 0){
        for(var c = 0; c < comment.comments.length; c++){
          if(comment.comments[c]._id.toString() === req.params.comment_id){
            comment_index = c;
            if(comment.comments[c].likes.length > 0){
              for(var l = 0; l < comment.comments[c].likes.length; l++){
                if(comment.comments[c].likes[l]._id.toString() === req.body.creator){
                  already_likes = true;
                }
              }
            }
          }
        }
      }
      // if(comment.comments[0].likes &&
      //   comment.comments[0].likes.length > 0){
      //   for(var i=0; i<comment.comments[0].likes.length; i++){
      //     if(comment.comments[0].likes[i]._id === req.body.creator)
      //       already_likes = true;
      //   }
      // }

      if(already_likes){
        var like_error = {
            status_code: 400,
            error_info: {
              error: 'already_liked',
              error_description: 'The requestor has already liked this post'
            }
          };
          _utils.prismResponse( res, null, false, like_error);

      }else{
        // if(comment_index)
        var like = { _id: req.body.creator};
        comment.comments[comment_index].likes.push(like);
        comment.comments[comment_index].likes_count++;
  /********************** END OPTIMIZED AREA51 DO IT **************************/

        comment.save(function(err, saved){
          if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
          var like_index = saved.comments[comment_index].likes_count -1;
          var response = { likes_count: saved.comments[comment_index].likes_count,
                           likes: saved.comments[comment_index].likes[like_index] };
          _utils.prismResponse(res, response, true);
        });
      }
    });

  }else{
    _utils.prismResponse(res, null, false, PrismResponse.invalidRequest);
  }
};

/**
 * [unlikeComment description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.unlikeComment = function(req, res){
  if(req.params.id && req.params.comment_id && req.body.creator){
    Post.findOne({
      _id: req.params.id,
      "comments._id": req.params.comment_id,
      "comments.likes._id": req.body.creator.toString()
    })
    .select('comments')
    .exec(function(error, comment){
      var unlike_error = {
          status_code: 400,
          error_info: {
            error: 'unable_to_unlike_comment',
            error_description: 'There are no qualifying likes available to unlike'
          }
        };

      if(error || !comment) _utils.prismResponse(res, null, false, PrismError.serverError);
      var did_unlike = false;
      var comment_index = null;
      if(comment.comments.length > 0){
        for(var c = 0; c < comment.comments.length; c++){
          if(comment.comments[c]._id.toString() === req.params.comment_id){
            comment_index = c;
            if(comment.comments[c].likes.length > 0){
              for(var l = 0; l < comment.comments[c].likes.length; l++){
                if(comment.comments[c].likes[l]._id === req.body.creator){
                  comment.comments[c].likes.splice(l, 1);
                  comment.comments[c].likes_count--;
                  did_unlike = true;
                }
              }
            }
          }
        }

      // if(comment.comments[0].likes.length > 0){
      //   var did_unlike = false;
      //   for(var i=0; i < comment.comments[0].likes.length; i++){
      //     if(comment.comments[0].likes[i]._id === req.body.creator.toString()){
      //       comment.comments[0].likes.splice(i, 1);
      //       comment.comments[0].likes_count--;
      //       did_unlike = true;
      //     }
      //   }

        if(!did_unlike){
          _utils.prismResponse(res, null, false, unlike_error);

        }else{
          comment.save(function(error, saved){
            if(error) _utils.prismResponse(res, null, false, PrismResponse.serverError);
            var response = {
              likes: [],
              likes_count: saved.comments[comment_index].likes_count
            };
            _utils.prismResponse(res, response, true);

          });
        }

      }else{
        _utils.prismResponse(res, null, false, unlike_error);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismResponse.invalidRequest);
  }
};

/**
 * Fetches a specific comment in the comments array
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.fetchComment = function(req, res){
  if(req.params.id && req.params.comment_id){
    Post.find({
      _id: req.params.id,
      "comments._id": req.params.comment_id
    })
    .select('comments.$')
    .populate('comments.creator', '_id first_name last_name name profile_photo_url')
    .exec(function(error, result){
      if(error) _utils.prismResponse(res, null, false, PrismError.invalidRequest);
      _utils.prismResponse(res, result[0].comments[0], true);
    });
  }else{
    _utils.prismResponse(res, null, false, PrismResponse.invalidRequest);
  }
};

/**
 * Destroys the requestors `like` association to the Post Object
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.unlikePost = function(req, res){
  if(req.params.id && req.body.creator){
    Post.findOne({_id: req.params.id}, function(err, post){
      if(err || !post){
        _utils.prismResponse( res,
                              null,
                              false,
                              PrismError.invalidRequest);
      }else{
        var removed = false;
        for(var i=0; i < post.likes.length; i++){
          if(post.likes[i]._id == req.body.creator){
            post.likes.splice(i, 1);
            removed = true;
          }
        }

        //if not removed send back as failure?
        if(!removed){
          _utils.prismResponse( res,
                          null,
                          false,
                          PrismError.invalidRequest);
        }else{
          post.likes_count = post.likes_count - 1;
          post.save(function(err, result){
            if(err){
              _utils.prismResponse( res,
                                    null,
                                    false,
                                    PrismError.serverError);
            }else{
              var response = {likes_count: result.likes_count,
                              likes: []};
              _utils.prismResponse( res, response, true);
            }
          });
        }
      }
    });

  }else{
    _utils.prismResponse( res,
                          null,
                          false,
                          PrismError.invalidRequest);
  }
};

/**
 * Fetchs a specific post object
 *
 * `Getting a post should return number of likes
 * AND if the requesting user has liked this post`
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.fetchPostAndLikeById = function(req, res){
  if(req.params.id && req.params.like_id){
    Post.findOne({_id: req.params.id, "likes._id": req.params.like_id},
                 {likes_count:1, "likes._id": 1},
                 function(err, post){

      if(err || !post){
         _utils.prismResponse( res,
                              null,
                              false,
                              PrismError.invalidRequest);
      }else{
        var response = {likes_count: post.likes_count,
                        likes: [{_id: post.likes[0]._id}]};
        _utils.prismResponse(res, response, true);
      }
    });

  }else{
    _utils.prismResponse( res,
                          null,
                          false,
                          PrismError.invalidRequest);
  }
};
