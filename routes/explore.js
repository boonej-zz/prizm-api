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
  }
};

var explore = function(req, res){
  var criteria = {status: 'active', scope: 'public', is_flagged: false};
  var twine = new Twine('Post', criteria, req, null, function(error, explore){
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
