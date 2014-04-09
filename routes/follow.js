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
    Twine         = require(_prism_home + 'classes/Twine'),
    Activity      = require(_prism_home + 'models/activity').Activity,
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
          }
        }

        if(is_following){
          var error = {
            status_code: 400,
            error_info: {
              error: 'unable_to_follow_user',
              error_description: 'The requested followee is already being followed'
            }
          };

          _utils.prismResponse(res, null, false, error);

        }else{

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
               
                new Activity({
                  action: 'follow',
                  to: req.params.id,
                  from: req.body.creator
                }).save(function(err, activity){
                  if(err){
                    _logger.log('error', 'an error recieved while creating a FOLLOW activity',
                                {err:err, activity:activity});
                    _utils.prismResponse(res, null, false, PrismError.serverError);
                  }else{
                    _logger.log('info', 'successfully created FOLLOW activity', {activty:activity});
                    _utils.prismResponse(res, {message: "Successfully followed "+req.params.id}, true);
                  }
                });
                //return response
                _utils.prismResponse(res, {message: 'Succesfully followed '+req.params.id}, true);
              });

            }else{
              _utils.prismResponse(res, null, false, PrismError.serverError);
            }
          });
        }
      }
    });
  }else{
    //create actual error
    _utils.prismResponse(res, null, false, PrismError.serverError);
  }
};

/**
 * [unfollow description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.unfollow = function(req, res){
  if(req.params.id && req.body.creator){
    //remove follower from followee
    User.findOne({_id: req.params.id}, function(err, result){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{
        //unset the creator from the followees followers array
        var is_following = true;
        for(var i=0; i < result.followers.length; i++){
          if(result.followers[i]._id == req.body.creator){
            result.followers.splice(i,1);
            //decrement the followers count
            result.followers_count = result.followers_count - 1;
            is_following = false;
          }
        }

        if(is_following){
          var error = {
            status_code: 400,
            error_info: {
              error: 'unable_to_unfollow_user',
              error_description: 'unable to process unfollow request'
            }
          };

          _utils.prismResponse( res,
                                null,
                                false,
                                error);
        }else{

          //save|update the followee record
          result.save(function(err, updated){
            if(err){
              _utils.prismResponse(res, null, falase, PrismError.serverError);

            }else{
              //remove followee from the follower
              User.findOne({_id: req.body.creator}, function(err, result){
                if(err){
                  _utils.prismRespnse(res, null, false, PrismError.invalidRequest);

                }else{

                  //unset the followee from the following array
                  for(var i=0; i < result.following.length; i++){
                    if(result.following[i]._id == req.params.id){
                      result.following.splice(i,1);
                      result.following_count = result.following_count -1;
                    }
                  }

                  result.save(function(err, updated){
                    if(err) {
                      _utils.prismResponse(res, null, false, PrismError.serverError);
                    }else{
                      //emit unfollow activity event
                      process.emit('activity', {
                        type: 'unfollow',
                        action: 'remove',
                        user: req.body.creator,
                        target: req.params.id,
                        object: updated
                      });
                      //send back successful unfollow
                      _utils.prismResponse(res, {message: 'Successfully unfollowed '+req.params.id}, true);
                    }
                  });
                }
              });
            }
          });
        }
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * [fetchFollowTypeById description]
 * @param  {[type]}   type [description]
 * @param  {[type]}   u_id [description]
 * @param  {[type]}   f_id [description]
 * @param  {Function} cb   [description]
 * @return {[type]}        [description]
 */
// var fetchFollowTypeById = function(type, u_id, f_id, cb){
//   if(type !== 'follower' || type !== 'following'){
//     cb(false);
//   }else{
//     var criteria, follow_identifier;
//   }
// };

/**
 * [fetchIsFollowingById description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.fetchIsFollowingById = function(req, res){
  if(req.params.id && req.params.following_id){
    var criteria = {
      _id: req.params.id,
      "following._id": req.params.following_id
    };
    var fetch = User.find(criteria);
    fetch.select({following: 1});
    fetch.exec(function(err, result){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{
        var response = (result.length !== 0)? result[0].following[0] : result;
        _utils.prismResponse(res, response, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};

/**
 * [fetchIsFollwersById description]
 * @param  {[type]} req [description]
 * @param  {[type]} res [description]
 * @return {[type]}     [description]
 */
exports.fetchIsFollowersById = function(req, res){
  if(req.params.id && req.params.follower_id){
    var criteria = {
      _id: req.params.id,
      "followers._id": req.params.follower_id
    };
    var fetch = User.find(criteria);
    fetch.select({followers:1});
    fetch.exec(function(err, result){

      if(err){
        _utils.prismResponse(res, null, false, PrismError.serverError);

      }else{
        var response = (result.length !== 0)? result[0].followers[0] : result;
        _utils.prismResponse(res, response, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
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
    new Twine('User', {_id: req.params.id}, req, {fields: 'following'}, function(err, result){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.ServerError);
      }else{
        if(result.data && result.data.length > 0)
          result.data = result.data[0].following;
        _utils.prismResponse(res, result, true);
      }
    });
    /**
    User.find({_id: req.params.id}, {following: 1}, function(error, user){
      if(error){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{
        var following_array = [];
        for(var i = 0; i < user[0].following.length; i++){
          following_array.push(user[0].following[i]._id);
        }

        User.find(
          {_id : {$in: following_array}},
          {first_name:1, last_name:1, profile_photo_url:1, posts_count:1},
          function(err, result){
            // console.log(result);
            _utils.prismResponse(res, result, true);
        });
      }
    });*/
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
    new Twine('User', {_id: req.params.id}, req, {fields: 'followers'}, function(err, result){
      if(err){
        _utils.prismResponse(res, null, false, PrismError.ServerError);
      }else{
        if(result.data && result.data.length > 0)
          result.data = result.data[0].followers;
        _utils.prismResponse(res, result, true);
      }
    });

    /**
    User.find({_id: req.params.id}, {followers : 1}, function(error, user){
      // debugger;
      if(error){
        _utils.prismResponse(res, null, false, PrismError.invalidUserRequest);

      }else{
        var followers_array = [];
        for(var i = 0; i < user[0].followers.length; i++){
          followers_array.push(user[0].followers[i]._id);
        }

        User.find(
          {_id: {$in : followers_array}},
          {first_name:1, last_name:1, profile_photo_url:1, posts_count:1},
          function(err, result){
            // console.log(result);
            _utils.prismResponse(res, result, true);
        });

        // _utils.prismResponse(res, user, true);
      }
    });*/
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};


