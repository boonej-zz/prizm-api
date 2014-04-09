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

exports.fetchUserActivity = function fetchUserActivity(req, res){
  if(req.params.id){
    var criteria = {to: req.params.id};
    new Twine('Activity', criteria, req, null, function(error, result){
      if(error) _utils.prismResponse(res, null, false, PrismError.serverError);
      _utils.prismResponse(res, result, true);
    });
  }else{
    _utils.prismResponse(res, null, false, PrismError.invalidRequest);
  }
};
