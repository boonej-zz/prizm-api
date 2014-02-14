/**
 * Gateway
 *
 * Handles decision points around porcessing request route
 * based on authorization, valid access_token, & route
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _prism_home   = process.env.PRISM_HOME
  , _auth_model   = require(_prism_home + 'models/auth')
  , _utils        = require(_prism_home + 'utils')
  , Code          = _auth_model.Code
  , Token         = _auth_model.Token
  , Client        = _auth_model.ClientApplication
  , Error         = require(_prism_home + 'error');

module.exports = function(req, res, next){
  var path = _utils.requestPathArray(req);
  if(needsAuthorization(path)){
    validateAuthorization(req, function(valid, err){
      if(!valid && err){
        _utils.prismResponse(res,
                            null,
                            false,
                            err,
                            err.status_code);
      }else if(!valid){
        // next();
      }else{
        next();
      }
    });
  }else{
    next();
  }
}

var needsAuthorization = function(req_path){
  var path = req_path;
  if(path && path[0] == 'oauth2' ){
    if(path.length == 2 && path[1] != 'login') return false;
  }
  return true;
}

var validateAuthorization = function(req, callback){
  _utils.authorizeClientRequest(req, function(err, valid, client){
    if(err && !valid) callback(false);
    if(valid){
      callback(true);
    }
    callback(false);
  });
}
