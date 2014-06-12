/**
 * Handles routing for a Users Activities
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose'),
    _prism_home = process.env.PRISM_HOME,
    _utils      = require(_prism_home + 'utils'),
    _logger     = require(_prism_home + 'logs.js'),
    PrismError  = require(_prism_home + 'error'),
    User        = require(_prism_home + 'models/user').User,
    Post        = require(_prism_home + 'models/post').Post,
    Twine       = require(_prism_home + 'classes/Twine'),
    Activity    = require(_prism_home + 'models/activity').Activity;

/**
 * Updates activities as "viewed"
 *
 * @param {String} user_id The "to" user identifier
 * @param {Object} headers The headers object (hash)
 */
var activityHasBeenViewed = function activityHasBeenViewed(user_id, results){
  if(results.length > 0){
    var date = results[0].create_date;
    var criteria = {
      to: user_id,
      has_been_viewed: false,
      create_date: {$lte: date}
    };

    var updateOptions = {
      $set: {has_been_viewed: true}
    };

    //update all records found by this criteria, setting viewed to true
    Activity.update(criteria, updateOptions, {multi:true}, function(err, updated){
      if(err) _logger.log('error',
                          'Activity update failed with error: ' + JSON.stringfiy(err));

      if(updated){
        _logger.log('info',
                    'Activity views successfully updated for user: '+user_id.toString()+
                    'for dates before : '+date);
        User.updateBadgeCount(user_id, 0, function(err, updated){
          if(err) _logger.log('error',
                              'Update Badge Count for user '+user_id + ' failed with error',
                              {err:err});
          if(updated) _logger.log('info', 'Successful UpdateBadgeCount for user ' + user_id);
        });
      }
    });
  }
};

/**
 * Fetchs a users activity notifications
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} req The response object
 */
exports.fetchUserActivity = function fetchUserActivity(req, res){
  if(req.params.id){
    var criteria = {to: req.params.id};
    new Twine('Activity', criteria, req, null, function(error, result){
      if(error) _utils.prismResponse(res, null, false, PrismError.serverError);
      if(Array.isArray(result.data)){
        activityHasBeenViewed(req.params.id, result.data);
      }
      _utils.prismResponse(res, result, true);
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};
