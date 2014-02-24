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

module.exports = function(req, res){
  var fetch_criteria = {},
      fetch_options = null,
      fetch_query;

  if(req.query && req.query.limit || req.query.feature_identifier){
    fetch_options = _utils.parsedQueryOptions(req.query);
    if(req.query.feature_identifier){
      if(req.query.direction){
        fetch_criteria = {scope: 'public', create_date: { $lt: req.query.feature_identifier}};

        }else{
          fetch_criteria = {scope: 'public', create_date: { $gt: req.query.feature_identifier}};
        }

        fetch_query = _utils.buildQueryObject(Post, fetch_criteria, fetch_options);
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
};
