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
  if(needsAuthorization(req)){
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
var needsAuthorization = function(req){
  var path = req.path.substr(1).split('/');
  console.log('the request is ' + path);
  if(path){
    if(path[0] == 'oauth2' && path.length == 2 && path[1] != 'login'){
      return false;
    } else if (path[0] == 'posts' && req.is('text/html') && path.length == 2) {
      return false;
    }
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
