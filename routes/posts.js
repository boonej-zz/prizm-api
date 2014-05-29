/**
 * Handles `Post` routing & management for all endpoints
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose         = require('mongoose'),
    _prism_home       = process.env.PRISM_HOME,
    _utils            = require(_prism_home + 'utils'),
    _logger           = require('winston'),
    _                 = require('underscore'),
    PrismError        = require(_prism_home + 'error'),
    Facebook          = require(_prism_home + 'classes/Facebook'),
    Twitter           = require(_prism_home + 'classes/Twitter'),
    User              = require(_prism_home + 'models/user').User,
    Post              = require(_prism_home + 'models/post').Post,
    ActivityListener  = require(_prism_home + 'classes/ActivityListener'),
    Activity          = require(_prism_home + 'models/activity').Activity,
    Twine             = require(_prism_home + 'classes/Twine'),
    Comment           = require(_prism_home + 'models/post').Comment;

/**
 * Fetchs Users Post Category Stats by Week & Year
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
exports.fetchCategoryPostCountByWeekAndYear = function(req, res){
  var user_id, week = null, offset = null, year = null, all_time = false, args;

  if(req.params.id){
    if(typeof(req.headers['x-arguments']) !== 'undefined'){
      var digest = new Buffer(req.headers['x-arguments'], 'base64').toString('utf8');
      args = JSON.parse(digest);
    }

    user_id = req.params.id;

    if(args){
      if(args.week && args.year && args.offset){
        year    = args.year;
        week    = args.week;
        offset  = args.offset;
      }
    }else{
      all_time = true;
    }

    Post.fetchCategoryPostCountByWeekAndYear(user_id, week, year, offset, function(err, result){
      _utils.prismResponse(res, formatCategoryByWeekAndYear(result, all_time), true);
    });

  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

var formatCategoryByWeekAndYear = function(result, all_time){
  var formatted, category, count, year, week, item;
  formatted = {};

  if(_.isArray(result)){

    if(result.length > 0){

      for(var index in result){
        item = result[index];

        if (all_time) {
          category = item._id.category;
          count = item.count;

          formatted[category] = count;

        }else{
          year = item._id.year;
          week = item._id.week + 1;
          category = item._id.category;
          count = item.count;

          if(!_.has(formatted, year)){
            formatted[year] = {};
            formatted[year][week]  = {};
            formatted[year][week][category] = count;

          }else{
            if(_.has(formatted[year], week)){
              if(!_.has(formatted[year][week], category)){
                formatted[year][week][category] = count;
              }

            }else{
              formatted[year][week] = {};
              formatted[year][week][category] = count;
            }
          }
        }
        count = null;
        category = null;
        year = null;
        week = null;
        item = null;
      }
    }
  }
  return formatted;
};

/**
 * Users Hashtags Stats for Category
*/
exports.fetchHashTagsForCategory = function(req, res){
  if(!req.params.id){
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }

  var category = false;

  if(typeof(req.body.category) !== 'undefined')
    category = req.body.category;

  Post.fetchHashtagsByCategory(req.params.id, category, function(err, result){
    if(err){
      _utils.prismResponse(res, null, false, PrismError.serverError);

    }else{
      _utils.prismResponse(res, formatHashTagsByCategory(result), true);
    }
  });
};

var formatHashTagsByCategory = function(result){
  var formatted, item, category, count, hashtag;
  formatted = {};
  if(_.isArray(result)){
    if(result.length > 0){

      for(var i in result){
        item = result[i];
        category = item._id.category;
        hashtag = item._id.hash_tags;
        count = item.count;

        if(!_.has(formatted, category)){
          formatted[category] = [];
        }

        var hash_count = {};
        hash_count.hash_tag = hashtag;
        hash_count.count = count;
        formatted[category].push(hash_count);
      }
    }
  }

  if(!_.isEmpty(formatted)){
    return sortAndCutFormattedHashTagsByCategory(formatted);
  }else{
    return formatted;
  }
};

var sortAndCutFormattedHashTagsByCategory = function(result){
  var sortHashTags = function(array){
    array.sort(function(a, b){
      if(a.count > b.count)
        return -1;
      if(a.count < b.count)
        return 1;
      return  0;
    });
    return array;
  };

  if(_.isObject(result)){
    for(var category in result){
      var sort_cat = result[category];
      sort_cat = _.first(sortHashTags(sort_cat), 5);
    }

  }else if(_.isArray(result)){
    result = _.first(sortHashTags(result), 5);
  }

  return result;
};


/**
 * Fetch Users Posts
 */
exports.fetchUserPosts = function(req, res){
  if(req.params.id){
    var ServerError = function(){
      _utils.prismResponse(res, null, false, PrismError.serverError);
    };

    var criteria = {
      $or: [
        {creator: req.params.id}, 
        {"tags._id" : req.params.id},
      ], 
      status: 'active',
      category: {$ne: 'accolade'}
    };
    new Twine('Post', criteria, req, null, function(error, result){
      if(error){
        _logger.log('error', 'Fetching Users Posts retunred error', {err:error});
        ServerError();
      }else{
        _utils.prismResponse(res, result, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequst);
  }
};

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

      Post.findOne({_id: req.params.id, status: 'active'}, function(err, post){
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

            //create activity
            _utils.registerActivityEvent(post.creator,
                                         req.body.creator,
                                         'comment',
                                         req.params.id,
                                         comment._id);
            //return response
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
 * Removes a post objects comment from the array
 *
 * `Note`: this DOES currently delete the array with no archive.
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
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
      "comments._id": req.params.comment_id,
      status: 'active'
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
 * Flags a post as innapropriate
 *
 * `Note`: if this is the 5th reported flag the status is updated from active to
 * review.
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.flagPost = function(req, res){
  if(req.params.id && req.body.reporter){
    Post.findOne({_id: req.params.id, status: 'active'}, function(err, post){
      if(err || !post){
        _utils.prismResponse(res, null, false, PrismError.invalidRequest);
      }else{
        var already_flagged = false;
        for(var i=0; i < post.flagged_reporters.length; i++){
          if(post.flagged_reporters[i].reporter_id === req.body.reporter.toString()){
            already_flagged = true;
          }
        }
        if(!already_flagged){
          post.flagged_reporters.push({reporter_id: req.body.reporter.toString(), create_date: Date.now()});
          post.save(function(err, saved){
            if(err || !saved){
              _utils.prismResponse(res, null, false, PrismError.serverError);
            }else{
              var res_message = {message: 'Post ' + post._id + ' Successfully Flagged'};
              _utils.prismResponse(res, res_message, true);
            }
          });
        }else{
          var flagged_error = {
            status_code: 400,
            error: {
              error: 'unable_to_flag_post',
              error_description: 'Requested reporter has already flagged this post.'
            }
          };
          _utils.prismResponse(res, null, false, flagged_error);
        }
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * Removes a post object (updates the status field from active to deleted)
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.removePost = function(req, res){
  var remove_error = {
    status_code: 400,
    error_info: {
      error: 'unable_to_delete_post',
      error_description: 'The requested post could not be deleted. '+
      'The requesting user does not have access or the post was already deleted'
    }
  };

  if(req.params.id && req.body.creator){
    Post.findOne({
      _id: req.params.id,
      creator: req.body.creator,
      status:'active'
    }, function(err, post){
      if(err || !post){
        _utils.prismResponse(res, null, false, remove_error);
      }else{
        post.status = 'deleted';
        post.delete_date = Date.now();
        post.save(function(error, saved){
          if(error) _utils.prismResponse(res, null, false, remove_error);
          //decrement the posts_count of the user
          User.findOne({_id: req.body.creator}, function(err, user){
            if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
            user.posts_count = user.posts_count -1;
            user.save(function(err, user_saved){
              if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
              _utils.prismResponse(res, {message: 'Post Successfully Removed'}, true);
            });
          });
        });
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * Updates the post object with availabilty to alter root document properties
 *
 * `updatable fields`: 'text', 'category', 'filepath', 'scope', 'location_name',
 *                      'location_longitude', 'location_latitude'
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.updatePost = function(req, res){
  var update_error = {
    status_code: 400,
    error_info: {
      error: 'unable_to_update_post',
      error_description: 'The requested post failed to update'
    }
  };

  if(req.params.id && Object.keys(req.body).length > 1 && req.body.creator){
    Post.findOne({
      _id: req.params.id,
      creator: req.body.creator,
      status: 'active'
    }, function( err, post){
      if(err || !post) _utils.prismResponse(res, null, false, update_error);
      if(typeof(req.body.text) !== 'undefined') post.text = req.body.text;
      if(typeof(req.body.category) !== 'undefined') post.category = req.body.category;
      if(typeof(req.body.filepath) !== 'undefined') post.filepath = req.body.filepath;
      if(typeof(req.body.scope) !== 'undefined') post.scope = req.body.scope;
      if(typeof(req.body.location_name) !== 'undefined')
        post.location_name = req.body.location_name;
      if(typeof(req.body.location_longitude) !== 'undefined')
        post.location_longitude = req.body.location_longitude;
      if(typeof(req.body.location_latitude) !== 'undefined')
        post.location_latitude = req.body.location_latitude;
      post.save(function(err, saved){
        if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
        _utils.prismResponse(res, {message: 'Post Successfully Updated'}, true);
      });
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
    var criteria = {_id: req.params.id, status: 'active'};
    var options = {
      fields: Comment.selectFields('basic'),
      is_child_model: true,
      child_model: 'Comment'
    };
    new Twine('Post', criteria, req, options, function(err, result){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.invalidRequest);
      }else{
        _utils.prismResponse(res, result, true);
      }
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
    Post.findOne({_id: req.params.id, status: 'active'}, function(err, post){
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

              //create activity
              _utils.registerActivityEvent(post.creator,
                                           req.body.creator,
                                           'like',
                                           post._id);

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
 * Fetch Posts Likes w/ populated shortUsers creator references
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.fetchPostLikes = function(req, res){
  if(req.params.id){

      var criteria = {_id: req.params.id, status: 'active'};
      new Twine('Post', criteria, req, {fields: ['likes','likes_count']}, function(err, post){
        if(err){
          _utils.prismResponse(res, null , false, PrismError.serverError);
        }else{
          if(!post){
            var likes_error = {
              status_code: 400,
              error_info:{
                error: 'unable_to_fetch_post_likes',
                error_description: 'post does not currently have any likes'
              }
            };
            _utils.prismResponse(res, null, false, likes_error);
          }else{
            for(var i=0; i < post.data.length; i++){
              post.data[i] = post.data[i].likes;
            }
            _utils.prismResponse(res, post, true);
          }
        }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * Fetch Post Comment Likes w/ populated shortUsers creator referencea
 *
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {Function}     The return values get forwarded to the
 *                        utility prismResponse method
 */
exports.fetchCommentLikes = function(req, res){
  if(req.params.id && req.params.comment_id){
    Post.find({
      _id: req.params.id,
      "comments._id": req.params.comment_id
    },
    {comments:1}, function(err, post){
      var comments_likes_error = {
        status_code: 400,
        error_info: {
          error: 'unable_to_fetch_comment_likes',
          error_description: 'The requested comment does not currently have any likes'
        }
      };

      if(err || !post){
        _utils.prismResponse(res, null, false, PrismError.serverError);

      }else{
        if(post[0].comments.length === 1 && post[0].comments[0].likes_count >0){
          var likes_users = [];
          var likes_user_ids = [];

          post[0].comments[0].likes.forEach(function(like){
            likes_user_ids.push(like._id);
          });
          if(likes_user_ids.length > 0){
            User.find({_id: {$in: likes_user_ids}}, function(err, users){
              if(users.length === likes_user_ids.length){
                users.forEach(function(user){
                  likes_users.push(user.shortUser());
                });
                var response = {likes_count: post[0].comments[0].likes_count,
                                likes: likes_users};
                _utils.prismResponse(res, response, true);

              }else{
                _utils.prismResponse(res, null, false, PrismError.serverError);
              }
            });
          }else{
            _utils.prismResponse(res, null, false, PrismError.serverError);
          }

        }else{
          _utils.prismResponse(res, null, false, comments_likes_error);
        }
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
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
      "comments._id": req.params.comment_id,
      status: 'active'
    })
    .select('comments creator')
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

          //create activity
          _utils.registerActivityEvent(comment.creator,
                                       req.body.creator,
                                       'like',
                                       req.params.id,
                                       req.params.comment_id);

          _utils.prismResponse(res, response, true);
        });
      }
    });

  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
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
      "comments.likes._id": req.body.creator.toString(),
      status: 'active'
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
      "comments._id": req.params.comment_id,
      status: 'active'
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
    Post.findOne({_id: req.params.id, status: 'active'}, function(err, post){
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
 * Fetchs a specific post object by like id
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
    Post.findOne({_id: req.params.id, "likes._id": req.params.like_id, status:'active'},
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

/**
 * Fetchs a specific post object by id
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 * @return {Function} The return values get forwarded to
 *                    utility prismResponse method.
 */
exports.fetchPostById = function(req, res){
  if(req.params.id){
    new Twine('Post', {_id: req.params.id}, req, null, function(err, post){
      //if err it is server related
      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);
        _logger.log('error',
                    'Error finding post by id',
                    {err:err, post_id:req.params.id});

      }else if(!post){
        var post_empty_error = {
          status_code: 400,
          error_info: {
            error: 'unable_to_find_post',
            error_description: 'the specified post id does not exist'
          }
        };

        _logger.log('error',
                    'unable to find post by id',
                    {result:post, post_id:req.params.id, error:post_empty_error});

        _utils.prismResponse(res, null, false, post_empty_error);

      }else{
        _logger.log('info', 'post_id '+req.params.id+' found via fetchPostById');
        _utils.prismResponse(res, post, true);

      }
    });
  }else{
    _utils.prismResponse( res, null, false, PrismError.invalidRequest);
  }
};
