/**
 * Handles routing and auth management for the client 
 * authroization endpoint
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose   = require('mongoose')
  , Token       = require(process.env.PRISM_HOME + 'models/auth').Token
  , Client      = require(process.env.PRISM_HOME + 'models/auth').ClientApplication
  , Code        = require(process.env.PRISM_HOME + 'models/auth').Code
  , Error       = require(process.env.PRISM_HOME + 'error');

/**
 * Auths Route module constructor & handler
 *
 * Checks to ensure the request is of type GET
 * then processes the request. If any other type of request
 * an error response is returned
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
module.exports = function(req, res){
  switch(req.route.method){
    case ('get'):
      createAuthorizationCode(req, res);
      break;
    default:
      res.send(400, 'Invalid Request');
      break;
  }
}

/**
 * Creates a new Authorization Code 
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
var createAuthorizationCode = function(req, res){
  var client_id     = req.query.client_id
    , response_type = req.query.response_type
    , redirect_uri  = req.query.redirect_uri;
  
  //check to make sure all 3 required params are provided.
  //if not, send error to redirect
  if(client_id && response_type && redirect_uri){
    //check if response type is "code", if not send error to redirect
    if(response_type == 'code'){
      //validate client exists, if not send error to redirect
      Client.findOne({client_id: client_id}, function(error, client){
        if(client && client.redirect_uri == redirect_uri){
          var newCode = new Code({client_id: client_id, redirect_uri: redirect_uri});
          newCode.save(function(error, authCode){
            //TODO: detect and invalidate all tokens associated with the code
            redirectSuccessfulAuthorizationCode(res, authCode, redirect_uri);
          });
        }else{
          redirectAuthorizationError(res, Error.unauthorized.error_info, redirect_uri);
        }
      });
    }
  }else{
    redirectAuthorizationError(res, Error.unsupportedResponseType.error_info, redirect_uri);
  }
}

/**
 * Redirects uncovered error to specified destination
 *
 * @param {HTTPResponse} res The response object
 * @param {Error} obj The Error object to return
 * @param {String} dest The destination to redirect the response object
 */
var redirectAuthorizationError = function(res, obj, dest){
  dest += '?error=' + obj.error + '&error_description=' + obj.error_description;
  res.redirect(dest);
}

/**
 * Redirects successful Authorization process to specified destination
 *
 * @param {HTTPResponse} res The response object
 * @param {Code} obj The Authrization Code created for the specified client_id
 * @param {String} dest The destination to redirect the successful response object
 */
var redirectSuccessfulAuthorizationCode = function(res, obj, dest){
  dest += '?code=' + obj.code + '&scope=' + 'all';
  res.redirect(dest);
}
