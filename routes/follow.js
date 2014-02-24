/**
 * Handles Follower/Following feature
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

exports.follow = function(req, res){
  if(req.body.creator && req.params.id){
    var follow_result = {followee: null, follower: null}, query, update_data;

    User.find({_id: {$in: [req.body.creator, req.params.id]}}, function(err, result){
      if(err){

        //TODO: create actual error response;
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{

        if(result.length !== 2){
          _utils.prismResponse(res, null, false, PrismError.serverError);
        }

        var follower, followee;
        if(result[0]._id == req.body.creator){
          follower = result[0];
          followee = result[1];
        }else{
          follower = result[1];
          followee = result[0];
        }

        //update followee record
        query = {_id: followee._id};
        update_data = {
          followers_count: followee.followers_count+1,
          $push: {
            "followers": {
              _id: follower._id.toString(),
              date: new Date().toString()
            }
          }
        };

        User.findOneAndUpdate(query,update_data, function(err, followee_update){

          if(followee_update){
            follow_result.followee = followee_update;

            //update the follower record
            query = {_id: follower._id};
            update_data = {
              following_count: follower.following_count+1,
              $push: {
                "following": {
                  _id: followee._id.toString(),
                  date: new Date().toString()
                }
              }
            };

            User.findOneAndUpdate(query, update_data, function(err, follower_update){
             if(err) _utils.prismResponse(res, null, false, PrismError.serverError);
              _utils.prismResponse(res, {}, true);
            });

          }else{
            _utils.prismResponse(res, null, false, PrismError.serverError);
          }
        });
      }
    });
  }else{
    //create actual error
    _utils.prismResponse(res, null, false, PrismError.serverError);
  }
};

exports.fetchFollowers = function(req, res){
  if(req.params.id){
    User.find({_id: req.params.id}, {'followers.$._id' : 1}, function(error, user){
      debugger;
      if(error){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{

      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};
