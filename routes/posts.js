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
        creator: req.body.creator
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

    var creator_populate = ['comments.creator', 'comments.creator.first_name comments.creator.last_name comments.creator.profile_photo_url comments.creator.name'];
    query.populate(creator_populate).exec(function(err, comm){
      if(err) _utils.prismResponse(res,null,false,PrismError.ServerError);

      var comments = comm[0].comments;
      var response = {comments: []};
      for(var i=0; i < comments.length; i++){
        var stripped = {
          creator: {_id: null, first_name:null,last_name:null,name:null,profile_photo_url:null},
          _id: null,
          text: null,
          create_date: null
        };

        stripped.creator._id = comments[i].creator._id;
        stripped.creator.first_name = comments[i].creator.first_name;
        stripped.creator.last_name = comments[i].creator.last_name;
        stripped.creator.name = comments[i].creator.name;
        stripped.creator.profile_photo_url = comments[i].creator.profile_photo_url;
        stripped.create_date = comments[i].create_date;
        stripped._id = comments[i]._id;
        stripped.text = comments[i].text;
        response.comments.push(stripped);
      }

      response._id = comm[0]._id;
      response.comments_count = comm[0].comments_count;
      _utils.prismResponse(res,response,true);

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






