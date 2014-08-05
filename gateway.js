/**
 * Gateway
 *
 * Handles decision points around porcessing request route
 * based on authorization, valid access_token, & route
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _prism_home   = process.env.PRISM_HOME,
    _auth_model   = require(_prism_home + 'models/auth'),
    _utils        = require(_prism_home + 'utils'),
    logger        = require(_prism_home + 'logs.js'),
    Code          = _auth_model.Code,
    Token         = _auth_model.Token,
    Client        = _auth_model.ClientApplication,
    PrismError    = require(_prism_home + 'error');

module.exports = function(req, res, next){
  var path = _utils.requestPathArray(req);
  if(needsAuthorization(path, req)){
    validateAuthorization(req, function(valid, err){
      if(err) logger.log('error','validateAuthroization returned an error:');
      if(!valid && err){
        _utils.prismResponse(res,
                            null,
                            false,
                            err,
                            err.status_code);
      }else if(valid){
        next();
      }else{
        // next();
      }
    });
  }else{
    next();
  }
};

/**
 * Checks to see if the route/request needs authentication
 *
 * @param  {String} req_path The request path
 * @return {Boolean}          Returns true/false based on path
 */
var needsAuthorization = function(req_path, req){
  var path = req_path;
  if(path && path[0] == 'oauth2' ){
    if(path.length == 2 && path[1] != 'login') return false;
  }
  if(path && path[0] == 'posts') {
    //if (req.get('Content-type') == 'text/html') return false;
    return false;
  }
  return true;
};

/**
 * Validates the authorization header
 *
 * @param  {HTTPRequest}   req The request object
 * @param  {Function} callback The callback function to be invoked
 * @return {Boolean}           Returns true/false in callback
 */
var validateAuthorization = function(req, callback){
  _utils.authorizeClientRequest(req, function(err, valid, client){
    if(err) logger.log('error','Utils authorize Client Request returned an error!',
      {error: err, is_valid: valid, for_client: client});
    if(err && !valid){
      callback(false);
    }else if(valid){
      callback(true);
    }else{
      callback(false);
    }
  });
};
