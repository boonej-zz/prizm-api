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

/**
 * [follow description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
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

        //check to make sure the followee is not already being followed by the follower
        var is_following = false;
        for(i=0; i < follower.following.length; i++){
          if(follower.following[i]._id.toString() == followee._id.toString()){
            is_following = true;
            break;
          }
        }

        if(is_following){
          var error = {
            status_code: 401,
            error_info: {
              error: 'unable_to_follow_user',
              error_description: 'The requested followee is already being followed'
            }
          };

          _utils.prismResponse(res, null, false, error);
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

/**
 * [fetchFollowing description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.fetchFollowing = function(req, res){
  if(req.params.id){
    User.find({_id: req.params.id}, {following: 1}, function(error, user){
      if(error){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{
        var following_array = [];
        for(var i = 0; i < user[0].following.length; i++){
          following_array.push(user[0].following[i]._id);
        }

        console.log(following_array);

        User.find(
          {_id : {$in: following_array}},
          {first_name:1, last_name:1, profile_photo_url:1, posts_count:1},
          function(err, result){
            console.log(result);
            _utils.prismResponse(res, result, true);
        });
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * [fetchFollowers description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.fetchFollowers = function(req, res){
  if(req.params.id){
    User.find({_id: req.params.id}, {followers : 1}, function(error, user){
      // debugger;
      if(error){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{
        var followers_array = [];
        for(var i = 0; i < user[0].followers.length; i++){
          followers_array.push(user[0].followers[i]._id);
        }

        console.log(followers_array);

        User.find(
          {_id: {$in : followers_array}},
          {first_name:1, last_name:1, profile_photo_url:1, posts_count:1},
          function(err, result){
            console.log(result);
            _utils.prismResponse(res, result, true);
        });

        // _utils.prismResponse(res, user, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};


