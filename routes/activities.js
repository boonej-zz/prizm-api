/**
 * Handles routing for a Users Activities
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose'),
    _prism_home = process.env.PRISM_HOME,
    _utils      = require(_prism_home + 'utils'),
    _logger     = require(_prism_home + 'logs.js'),
    PrismError  = require(_prism_home + 'models/user').User,
    User        = require(_prism_home + 'models/post').Post,
    Twine       = require(_prism_home + 'classes/Twine'),
    Activity    = require(_prism_home + 'models/activity').Activity;

/**
 * Updates activities as "viewed"
 *
 * @param {String} user_id The "to" user identifier
 * @param {Object} headers The headers object (hash)
 */
var activityHasBeenViewed = function activityHasBeenViewed(user_id, headers){
  if(headers['x-arguments']){
    var args = new Buffer(headers['x-arguments'], 'base64').toString('utf8');
    args = JSON.parse(args);

    if(args.page && args.has_been_viewed === false){
      var criteria = {
        to: user_id,
        has_been_viewed: false,
        create_date: {$lt: args.page}
      };

      var updateOptions = {
        $set: {has_been_viewed: true},
        multi: true
      };

      //update all records found by this criteria, setting viewed to true
      Activity.update(criteria, updateOptions, function(err, updated){
        if(err) _logger.log('error',
                            'Activity update failed with error: ' + JSON.stringfiy(err));

        if(updated) _logger.log('info',
                                'Activity views successfully updated for user: '+user_id.toString()+
                                'for dates before : '+args.page);
      });
    }
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
      activityHasBeenViewed(req.params.id, req.headers);
      _utils.prismResponse(res, result, true);
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};
