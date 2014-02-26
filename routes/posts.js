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
    Post          = require(_prism_home + 'models/post').Post;

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
          _utils.prismResponse( res, {}, true);

        }else{



          post.likes.push({"_id":req.body.creator});
          post.likes_count = (post.likes !== null) ? post.likes_count + 1 : 1;
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
            post.likes.slice(i, 1);
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
    Post.findOne({_id: req.params.id, "likes._id": like_id},
                 {likes_count:1, "likes._id": 1},
                 function(err, post){
      debugger;
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






