/**
 * Handles routing and token management for oauth2/token endpoint
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , Error         = require(process.env.PRISM_HOME + 'error')
  , _auth_model   = require(process.env.PRISM_HOME + 'models/auth')
  , Client        = _auth_model.ClientApplication
  , Token         = _auth_model.Token
  , Code          = _auth_model.Code;

/**
 * Token Route module constructor
 *
 * Checks to ensure the HTTP request is of type POST
 * then processes the request. If any other type of request
 * an error response is returned
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
module.exports = function(req, res){
  switch(req.route.method){
    case 'post':
      createAuthorizationToken(req, res);
      break;
    default:
      res.send(400, Error.invalidRequest.error_info);
      break;
  }
}

/**
 * Checks for Basic Authorization header & decodes credentials
 *
 * @param {String} header The Authorization header string value
 * @return {Array} Returns an array of user:pass passed in Basic Auth Header
 */
var credentials = function(header){
  if(header){
    var authArray = header.split(" ");
    if(authArray.length == 2 && authArray[0] == 'Basic'){
      var creds = new Buffer(authArray[1], 'base64').toString('ascii');
      var credsArray = creds.split(':');
      if(credsArray.length == 2){
        return credsArray;
      }
    }
  }
  return false;
}

var createAuthorizationToken(req, res){
  var grant_type    = req.body.grant_type
    , code          = req.body.code
    , redirect_uri  = req.body.redirect_uri
    , refresh_token = req.body.refresh_token;

  switch(grant_type){
    case 'authroization_code':
      handleAuthorizationCode(req, res);
      break;
    case 'client_credentials':
      //TODO
      break;
    case 'refresh_token':
      handleRefreshToken(req, res);
      break;
    default:
      res.send(400, Error.invalidRequest.error_info);
      break;
  } 
}

var handleAuthorizationCode(req, res){
  var code          = req.body.code
    , redirect_uri  = req.body.redirect_uri
  if(code && redirect_uri){
    Code.findOne({code: code}, function(error, authCode){
      if(authCode){
        Client.findOne({client_id: authCode.client_id}, function(error, client){
          if(client){
            var creds = credentials(req.get('Authorization'));
            if(client.client_id == creds[0] && client.client_secret == creds[1]){
              Token.remove({code: code}, function(error){
                if(error) console.log(error);
                if(authCode.redirect_uri == redirect_uri){
                  var token = new Token({grant_type: 'authorization_code', 
                                        client_application: client, 
                                        code: code});

                  token.save(function(error, newToken, count){
                    res.send(newToken.cleanJSON());
                  });
                }else{
                  res.send(401, Error.unauthorized.error_info);
                }
              });
            }else{
              res.send(401, Error.unauthorized.error_info);
            }
          }else{
            res.send(400, Error.invalidRequest.error_info);
          }
        });
      }else{
        res.send(400, Error.invalidRequest.error_info);
      }
    });
  }else{
    res.send(400, Error.invalidRequest.error_info);
  }
}
