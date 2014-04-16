/**
 * Handles routing & management for the Explore feature
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _             = require('underscore'),
    _prism_home   = process.env.PRISM_HOME,
    _utils        = require(_prism_home + 'utils'),
    _logger       = require(_prism_home + 'logs.js'),
    Twine         = require(_prism_home + 'classes/Twine'),
    PrismError    = require(_prism_home + 'error'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post;

var BASE_EXPLORE_CRITERIA = {scope: 'public', status: 'active'};

/**
 * BETTER AGGREGATE FOR HASHTAG GROUPING & COUNTS
 *
 * $unwind: "$has_tags"
 * $match: {hash_tags: regex, status:active}
 * $group: {_id: $hash_tags, count: {$sum :1}}
 * $sort: {count: -1}
 * $limit: 30
 */

var searchHashTags = function(req, res){
  if(!req.params.hash){
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);

  }else{
    var regex_hash_string = new RegExp(req.params.hash, 'gi');
    Post.aggregate([
      { $unwind: "$hash_tags" },
      { $match: { hash_tags: regex_hash_string, status: 'active', scope: 'public' }},
      { $group: { _id: "$hash_tags", count: {$sum :1} }},
      { $sort: { count: -1 }},
      { $limit: 30 } ],
      function(err, hashtags){
        if(err){
          debugger;
          _utils.prismResponse(res, null, false, PrismError.invalidRequest);

        }else{

          //reformat the results properly {hash_tag: '', count: }
          var response = [];
          for(var index in hashtags){
            var item = hashtags[index];
            response.push({hash_tag: item._id, count: item.count});
          }
          _utils.prismResponse(res, response, true);
        }
    });

/**
    Post.aggregate([
      {$unwind: "$hash_tags"},
      {$match: {hash_tags: regex_hash_string, status: 'active'}},
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
    },
    {$limit: limit}
    ], function(err, result){
      var sorted = [];
      var formated = {};
      if(err){
        cab(err, null);

      }else{
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
        for(var i =0; i < sorted.length; i++){
          var withhash = (hash_tag.indexOf('#') === -1)? '#'+hash_tag : hash_tag;
          var matched  =false;
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
      }
    });
**/
  }
};

var explore = function(req, res){
  new Twine('Post', BASE_EXPLORE_CRITERIA, req, null, function(error, explore){
    if(error){
      _utils.prismResponse(res, null, false, PrismError.serverError);
    }else{
      _utils.prismResponse(res, explore, true);
    }
  });
};

var thisExports = {
  explore:  explore,
  search:   searchHashTags
};

module.exports = thisExports;
