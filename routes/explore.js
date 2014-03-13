/**
 * Handles routing & management for the Explore feature
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

var sortByCreateDate = function(a,b){
  if(a.create_date.valueOf() < b.create_date.valueOf())
    return 1;
  if(a.create_date.valueOf() > b.create_date.valueOf())
    return -1;
  return 0;
};

var searchHashTags = function(hash_tag, cb){
  var regex_hash_string = new RegExp(hash_tag, 'gi');
  Post.aggregate([
    {$unwind: "$hash_tags"},
    {$match: {hash_tags: regex_hash_string}},
    {$group: {
      _id:{
        "_id":"$_id",
        "text":"$text",
        "file_path":"$file_path",
        "create_date":"$create_date",
        "target_id":"$target_id",
        "creator":"$creator",
        "comments_count":"$comments_count",
        "likes_count":"$likes_count"
      },
      hash_tags:{
        $push:"$hash_tags"
      }
    }
  }
  ], function(err, result){
    var sorted = [];
    var formated = {};
    result.forEach(function(post){
      formated._id = post._id._id;
      formated.text = post._id.text;
      formated.file_path = post._id.file_path;
      formated.create_date = post._id.create_date;
      formated.target_id = post._id.target_id;
      formated.comments_count = post._id.comments_count;
      formated.likes_count = post._id.likes_count;
      formated.hash_tags = post.hash_tags;
      formated.creator = post._id.creator;
      sorted.push(formated);
      formated = {};
    });

    var exact_match = [];
    var unmatched = [];
    console.log(sorted);
    for(var i =0; i < sorted.length; i++){
      var withhash = (hash_tag.indexOf('#') === -1)? '#'+hash_tag : hash_tag;
      var matched  =false;
      console.log(sorted[i]);
      if(sorted[i].hash_tags.indexOf(withhash) !== -1){
        exact_match.push(sorted[i]);

      }else if(sorted[i].hash_tags.indexOf(hash_tag) !== -1 && !matched){
        exact_match.push(sorted[i]);

      }else{
        unmatched.push(sorted[i]);
      }
    }
    sorted = [];
    if(unmatched.length > 0) unmatched.sort(sortByCreateDate);
    if(exact_match.length > 0){
      exact_match.sort(sortByCreateDate);
      sorted.push(exact_match);
    }
    sorted = exact_match.concat(unmatched);
    err = (!err)? null:err;
    cb(err, sorted);
  });
};

var fetchShortUserById = function(user_id, cb){
  User.findOne({_id: user_id}, function(err, result){
    if(err || !result){
      cb(err, null);
    }else{
      var short_user = {
        _id: result._id,
        name: result.name,
        first_name: result.first_name,
        last_name: result.last_name,
        profile_photo_url: result.profile_photo_url
      };
      cb(null, short_user);
    }

  });
};

module.exports = function(req, res){
  var fetch_criteria = {},
      fetch_options = null,
      fetch_filter,
      fetch_query;
  if(req.query.hash_tags){
    searchHashTags(req.query.hash_tags, function(err, result){
      if(err || !result){
        _utils.prismResponse(res,null,false,PrismError.serverError);

      }else{
        if(result.length > 0 ){
          var user_ids = [];
          result.forEach(function(post){
            user_ids.push(post.creator.toString());
          });

          User.find({_id: {$in :user_ids}}, function(err, users){
            if(users.length === user_ids.length){
              for(var i=0; i < users.length; i++){
                for(var p = 0; p < result.length; p++){
                  if(result[p].creator.toString() === users[i]._id.toString()){
                    result[p].creator = users[i].shortUser();
                  }
                }
              }
              _utils.prismResponse(res, result, true);
            }else{
              _utils.prismResponse(res, null, false, PrismError.serverError);
            }
          });

        }else{
          _utils.prismResponse(res, {message: 'Unable to find posts with hashtag: ' +req.query.hash_tags}, true);
        }
      }
    });
  }else{

    if(req.query && req.query.limit || req.query.feature_identifier){
      fetch_options = _utils.parsedQueryOptions(req.query);
      if(req.query.feature_identifier){
        if(req.query.direction){
          fetch_criteria = { scope: 'public',
                             create_date: { $lt: req.query.feature_identifier } };

          }else{
            fetch_criteria = { scope: 'public',
                               create_date: { $gt: req.query.feature_identifier } };
          }

          if(typeof(req.query.location_name) !== 'undefined')
            fetch_criteria.location_name = req.query.location_name;

          fetch_query = _utils.buildQueryObject(  Post,
                                                  fetch_criteria,
                                                  fetch_options );
        }

    }else{
      if(!fetch_options) fetch_options = _utils.parsedQueryOptions(req.query);
      if(fetch_criteria == {}) fetch_criteria = {scope: 'public'};
      fetch_query = _utils.buildQueryObject(Post, fetch_criteria, fetch_options);
    }

    var fetch_populate = ['creator', 'first_name last_name profile_photo_url'];
    fetch_query.populate(fetch_populate).exec(function(error, explore){
      if(error){
        _utils.prismResponse(res, null, false, PrismError.serverError);
      }else{
        _utils.prismResponse(res, explore, true);
      }
    });
  }
};
