/**
 * Tokens
 *
 * Handles routing and token management for oauth2/token endpoint
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    Error         = require(process.env.PRISM_HOME + 'error'),
    _auth_model   = require(process.env.PRISM_HOME + 'models/auth'),
    Client        = _auth_model.ClientApplication,
    Token         = _auth_model.Token,
    Code          = _auth_model.Code,
    _utils        = require(process.env.PRISM_HOME + 'utils');

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
};

/**
 * Checks for Basic Authorization header & decodes credentials
 *
 * @param {String} header The Authorization header string value
 * @return {Array} Returns an array of user:pass passed by  Basic Auth headers
 */
var credentials = function(header){
   console.log(header);
   if(header){
    var authArray = header.split(" ");
    if(authArray.length == 2 && authArray[0] == 'Basic'){
      var creds = new Buffer(authArray[1], 'base64').toString('ascii');
      var credsArray = creds.split(':');
      if(credsArray.length == 2){
        console.log(credsArray);
        return credsArray;
      }
    }
  }
  return false;
};

/**
 * Checks to ensure the client is authorzied to make
 * any token request by validating there auth header
 * with validation against a valid client record in the db
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 * @param {Function} callback The callback function invoked
 */
var authorizeClientRequest = function(req, res, callback){
  var creds = credentials(req.get('Authorization'));
  console.log(creds);
  if(creds){
    Client.findOne({client_id: creds[0], client_secret: creds[1]}, function(error, result){
      if(error){
        callback(error, false, result);
      }else{
        callback(error, true, result);
      }
    });
  }else{
    console.log('No credentials.');
    callback('Error parsing credentials', false, null);
  }
};

/**
 * Constructor handler based on grant_type
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
var createAuthorizationToken = function(req, res){
  console.log('creating token');
  var grant_type    = req.body.grant_type,
      code          = req.body.code,
      redirect_uri  = req.body.redirect_uri,
      refresh_token = req.body.refresh_token;

  switch(grant_type){
    case 'authorization_code':
      console.log('Has auth code');
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
};

/**
 * Handles token creation logic based on `authorization_code`
 * auth grant_type
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} res The response object
 */
var handleAuthorizationCode = function(req, res){
  var code          = req.body.code,
      redirect_uri  = req.body.redirect_uri;
  console.log(code);
  console.log(redirect_uri);
  if(code && redirect_uri){
    Code.findOne({code: code}, function(error, authCode){
      if(authCode){
        Client.findOne({client_id: authCode.client_id}, function(error, client){
          if(client){
            var creds = credentials(req.get('Authorization'));
            console.log(client);
            console.log(client.client_id + '=' + creds[0]);
            console.log(client.client_secret + '=' + creds[1]);
            if(client.client_id == creds[0] && client.client_secret == creds[1]){
              console.log('Credentials match');
              Token.remove({code: code}, function(error){
                if(error) console.log(error);
                if(authCode.redirect_uri == redirect_uri){
                  var token = new Token({grant_type: 'authorization_code',
                                        client_application: client,
                                        code: code});

                  token.save(function(error, newToken, count){
                    _utils.prismResponse(res, newToken.cleanJSON(), true);
			//res.send(newToken.cleanJSON());
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
};

/**
 * Handles token creation logic based on `refresh_token` auth grant_type
 *
 * @param {HTTPRequest} req The request object
 * @param {HTTPResponse} req The response object
 */
var handleRefreshToken = function(req, res){
  var creds = credentials(req.get('Authorization'));
  Client.findOne({client_id: creds[0], client_secret: creds[1]}, function(error, client){
    // console.log("handleRefreshToken :" + JSON.stringify(client));
    if(client){
      refreshToken(client, req.body.code, function(result){
        // console.log("refresh token result: " + result + "refresh error: " + error);
        if(result){
          _utils.prismResponse(res, result.cleanJSON(), true);
        }else{
          _utils.prismResponse(res, null, false, Error.unauthorizedClient, Error.unauthorizedClient.status_code);
        }
       });
    }else{
      _utils.prismResponse(res, null, false, Error.unauthorizedClient, Error.unauthorizedClient.status_code);
      //res.send(401, Error.unauthorizedClient.error_info);
    }
  });
};

//TODO: handle straight up client credentials auth --
//if we even want to authenticate this way.
//var handleClientCredentials = function(req, res){}

/**
 * Refresh token method creates a new token and removes existing one
 * from the db
 *
 * @param {Object} client The ClientApplication Model object
 * @param {String} refresh The refres_token string
 * @param {Function} next The callback iterator invoked
 */
var refreshToken = function(client, refresh, next){
  // console.log("client: " + client + "regresh :" + JSON.stringify(refresh));
  Token.findOne({refresh_token: refresh, client_application: client}, function(error, result){
    // console.log("rrefresh token log: " + JSON.stringify(result));
    if(error){
      console.log(error);
      next(false);
    }else if(result){
      var token = new Token({ grant_type: result.grant_type,
                              client_application: client,
                              code: result.code });
      token.save(function(error, newToken, count){
        if(error){
          console.log(error);
          next(false);
        }else{
          Token.remove({_id: result._id}, function(error){
            if(error) console.log(error);
            next(newToken);
          });
        }
      });
    }else{
      next(false);
    }
  });
};
