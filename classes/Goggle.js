/**
 * Google social auth & integration
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _config     = require('config'),
    _request    = require('request'),
    _home       = process.env.PRISM_HOME,
    _utils      = require(_home + 'utils'),
    _logger     = require(_home + 'logs'),
    PrismError  = require(_home + 'error'),
    User        = require(_home + 'models/user');

/**
 * Google+ Class
 *
 * Handles Google+ user functionality & authorization
 *
 * @param {String} g_id The Google+_ user identifier
 * @param {String} g_access_token The Google+ access_token passed from mobile
 */
function Google(g_access_token, g_id){
  this.identifier     = g_id;
  this.token          = g_access_token;
  this.client_id      = _config.social.google.client_id;
  this.client_secret  = _config.social.google.client_secret;
}

Google.prototype.authorizeUser = function authorizeUser(callback){
  if(this.token){
    //attempt to fetch the user profile with supplied accesstoken
    _request({
      url: _config.social.goolge.authorize_uri+this.token,
      json: true,
      method: "GET"
    }, function(err, res, body){
      if(err) 
        _logger.log('error', 'Unable to authenticate with google plus', {err: err});

      if(res.statusCode !== 200){
        _logger.log('error', 'Unable to authenticate wiht google plus: '+
                    'Invalid Access Token', 'statuscode returned: '+res.statusCode);
        callback(new Error('Invalid Access Token with status_code: '+res.statusCode));

      }else{
        _logger.log('info', 'Succesful google plus authorization', body);
        callback(err, JSON.parse(body));
      }

    });
  }else{
    callback(new Error('The google identifier and token are required to authenticate'));
    _logger.log('error', 'Invalid google+ request, missing id & token');
  }
};
