/**
 * Handles routing & management for /user* endpoints
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _prism_home   = process.env.PRISM_HOME
  , _utils        = require(_prism_home + 'utils')
  , _logger       = require('winston')
  , Error         = require(_prism_home + 'error')
  , Facebook      = require(_prism_home + 'classes/Facebook')
  , Twitter       = require(_prism_home + 'classes/Twitter')
  , User          = require(_prism_home + 'models/user').User;

/**
 * TODO: pull logging for errors out into error class (which needs refactoring)
 */

/**
 * Handles authenticating user login request
 *
 * TODO: create session on success & add logout to destroy session
 * 
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {User} Returns a valid user object
 */
exports.login = function(req, res){
  if(isValidLoginRequest(req.body)){
    if(isSocialProvider(req.body)){
      handleSocialProviderLogin(req.body, function(error, result){
        if(error || result === false){
          //social login failure - user does not exist or 
          //failire to authenticate with social provider
          if(error){
          	_utils.prismResponse( res,
                	                null,
                        	        false,
                                	error,
                                	error.status_code);
      	  }else{
      		_utils.prismResponse( res, null, false, Error.invalidLoginUserDoesNotExist, 
                                                  Error.invalidLoginUserDoesNotExist.status_code);	
      		}
        }else{
          //succesful login - send back returned user object
          var user = result.toObject();
          delete user.posts;
          delete user.likes;
          delete user.comments;
          delete user.provider_token;
          if(!typeof(user.provider_token_secret) == 'undefined') delete user.provider_token_secret;
          _utils.prismResponse( res, user , true);
        }
      });
    }else{
      User.findOne({email: req.body.email}, function(error, result){
        if(error){
          _utils.prismResponse( res, 
                                null,
                                false,
                                Error.invalidLoginUserDoesNotExist,
                                Error.invalidLoginUserDoesNotExist.status_code );
        }else if(result){
          if(hashAndValidatePassword(result, req.body.password)){
            _utils.prismResponse(res, result, true, null, null);
          }else{
	         _utils.prismResponse( res, 
                                 null, 
                                false,
                                Error.invalidUserCredentials,
                                Error.invalidUserCredentials.status_code );
          } 
        }else{
          _utils.prismResponse( res, 
                                null,
                                false,
                                Error.invalidLoginUserDoesNotExist,
                                Error.invalidLoginUserDoesNotExist.status_code );
        }
      });
    }
  }else{
    _utils.prismResponse( res, 
                          null, 
                          false,
                          Error.invalidLoginRequest,
                          Error.invalidLoginRequest.status_code );
  }
}

/**
 * Handles User creation if user does not exist
 * 
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {User} Returns the newly created User object
 */
exports.register = function(req, res){
  if(isValidRegisterRequest(req)){
    //Handle traidtional registration -- 
    var newUser = new User({
      first_name: req.body.first_name,
      last_name: req.body.last_name,
      email: req.body.email,
      gender: req.body.gender,
      zip_postal: req.body.zip_postal,
      city: req.body.city,
      state: req.body.state,
      birthday: req.body.birthday
    });

    if(typeof(req.body.password) != 'undefined') newUser.password = req.body.password;

    if(typeof(req.body.cover_photo_url) != 'undefined') newUser.cover_photo_url = req.body.cover_photo_url;

    if(typeof(req.body.profile_photo_url) != 'undefined') newUser.profile_photo_url = req.body.profile_photo_url;
    
    //check, validate, & handle social registration
    if(isSocialProvider(req.body)){
      handleSocialProviderRegistration(req.body, function(error, social){
        // console.log('error/social returned from handle in reg' + error + social);
        if(error && social === false){
          _utils.prismResponse( res, null, false, error, error.status_code);
        }else if(social){
          newUser.provider = req.body.provider;
          newUser.provider_token = req.body.provider_token;
          newUser.provider_id = social.id;
          if(newUser.provider == 'twitter'){
            newUser.provider_token_secret = req.body.provider_token_secret;
          }
          // console.log('saving social user: ' + JSON.stringify(newUser));
          newUser.save(function(error, result){

            if(error || !result){
               _utils.prismResponse( res, 
                                null, 
                                false, 
                                Error.invalidRegisterUserExists, 
                                Error.invalidRegisterUserExists.status_code);
            }else{
              // console.log("cleanUserJSON : " + JSON.stringify(result.cleanUserJSON()));
              // console.log('just straightup result :' + JSON.stringify(result));
              _utils.prismResponse(res, result, true);
            }

          });
        }else{
          _utils.prismResponse( res, null, false, Error.serverError, Error.serverError.status_code);
        }
      });
    }else{

      newUser.save(function(error, result){
        if(error || !result){
          _utils.prismResponse( res, 
                                null, 
                                false, 
                                Error.invalidRegisterUserExists, 
                                Error.invalidRegisterUserExists.status_code);
        }else{
          var user = result.toObject();
          delete user.password;
          delete user.posts;
          delete user.likes;
          delete user.comments;

          _utils.prismResponse(res, result, true);
        }
      });
    }

  }else{
    _utils.prismResponse(res, null, false, Error.invalidRequest, Error.invalidRequest.status_code);
  }
}

/**
 * Fetchs Prism user object by identifier
 * 
 * @param  {HTTPRequest} req The request object
 * @param  {HTTPResponse} res The response object
 * @return {User} Returns the valid user object to the response object
 */
exports.fetchUser = function(req, res){
  if(req.params.id){
    User.findOne({_id: req.params.id}, function(error, result){
      if(error){
        console.log('Error retrieving user by id: ' + req.params.id);
        _utils.prismResponse(res, null, false, Error.invalidUserRequest, Error.invalidUserRequest.status_code);
      }else{
        var user = result.toObject();
          if(!typeof(user.password) == 'undefined') delete user.password;
          if(!typeof(user.provider_token) == 'undefined') delete user.provider_token;
          if(!typeof(user.provider_token_secret) == 'undefined') delete user.provider_token_secret;
          delete user.posts;
          delete user.likes;
          delete user.comments;

        _utils.prismResponse(res, user, true);
      }
    });
  }else{
    _utils.prismResponse(res, null, false, Error.invalidUserRequest, Error.invalidUserRequest.status_code);
  }
}

/**
 * Validates the required User properties are present 
 * for registration
 * 
 * @param  {HTTPRequest} req The request object
 * @return {Boolean}
 */
var isValidRegisterRequest = function(req){
  if( typeof(req.body.first_name) == 'undefined'  ||
      typeof(req.body.last_name) == 'undefined'   ||
      // typeof(req.body.email) == 'undefined'     ||
      typeof(req.body.gender) == 'undefined' ||
      typeof(req.body.zip_postal) == 'undefined' ||
      typeof(req.body.city) == 'undefined' ||
      typeof(req.body.state) == 'undefined' ||
      typeof(req.body.birthday) == 'undefined' ){
  
    return false;

  }else{
    if(!isSocialProvider(req.body)){
      if(typeof(req.body.email) == 'undefined'){
        return false;
      }
    }
    return true;
  }
}

/**
 * [isValidSocialRegisterRequest description]
 * @param  {[type]}  req
 * @return {Boolean}
 */
var isValidSocialRegisterRequest = function(req){
  if(isValidRegisterRequest(req)){
    if( typeof(req.body.provider) == 'undefined' ||
        typeof(req.body.provider_token) == 'undefined'  ){
      return false;
    }else{
      if(req.body.provider == 'twitter'){
        if(typeof(req.body.provider_token_secret) == 'undefined') return false;
      }

      return true;
    }
  }else{
    return false;
  }
}

/**
 * Takes passed plain text password & hashes it to
 * validate it equals the stored hash password
 * 
 * @param  {User} user The located User object
 * @param  {String} password_to_validate The password to validate 
 * @return {Boolean} 
 */
var hashAndValidatePassword = function(user, password_to_validate){
  //create user hash
  if(user){
    var hash_salt             = user.createUserSalt();
    var password_to_validate  = _utils.prismEncrypt(password_to_validate, hash_salt);
    if(password_to_validate == user.password) return true;
  }
  return false;
}

/**
 * [handleSocialProvider description]
 * @param  {[type]}   body
 * @param  {Function} callback
 * @return {[type]}
 */
var handleSocialProviderLogin = function(body, callback){
  switch(body.provider){
    case 'facebook':
      var fb = new Facebook(body.provider_token);
      fb.authorizeUser(function(error, response){
        if(error){
          // console.log('authorize on login did error: ' + error);
          callback(error, false);
        }else if(response){
          // console.log('authorize did have response. checking user with: ' + JSON.stringify(response.body));
          User.findOne({provider_id: response.body.id}, function(error, response){
            // console.log('find user social provoder -- error: ' + error + ' && response: ' + JSON.stringify(response) );
            if(error){
              callback(Error.invalidSocialUser, false);
            }else if(response && response._id){
              callback(false, response);
            }else{
              callback(Error.invalidSocialUser, false);
            }
          });
        }else{
          callback(Error.serverError, false);
        }
      });
      break;
    case 'twitter':
      var tw = new Twitter(body.provider_token, body.provider_token_secret);
      tw.authorizeUser(function(error, result){
        if(error){
          _logger.error('Error returned attempting to authorize twitter user: ', 
                    error);
          callback(error, false);

        }else if(result){
          _logger.info('Succesfuly returned object from authorizing twitter user: ', result);
          _logger.log(result);

          User.findOne({provider_id: result.id.toString()}, function(error, response){
            if(error){
              _logger.error('Error returned trying to find twitter user in prism.'
                            +' Users does not exist.', {error: error, twitter_user: result});
              callback(Error.invalidSocialUser, false);
            
            }else if(response && response._id){
              _logger.info('Found twitter user to validate login', {user: response});
              callback(false, response);
            
            }else{
              _logger.warn('Did not find an error or result in fetching twitter user');
              callback(Error.invalidSocialUser, false);
            }
          });

        }else{
          _logger.error('A server error occured. No error or' 
                      + ' result was retured from authorizing a twitter user');
          callback(Error.serverError, false);
        }
      });
      break;
    default:
      _logger.log('A unsupported provider type was passed to user registration ',
                  {provider: body.provider});
      
      callback(Error.unsupportedProviderType(body.provider), false);
      break;
  }
}

/**
 * [handleSocialProviderRegistration description]
 * @param  {[type]}   body     [description]
 * @param  {Function} callback [description]
 * @return {[type]}            [description]
 */
var handleSocialProviderRegistration = function(body, callback){
  switch(body.provider){
    case 'facebook':
      var fb = new Facebook(body.provider_token);
      fb.authorizeUser(function(error, response){
        if(error){
          _logger.log('fb authorize error in handleRegistration :',
                      {error: error});

          callback(error, false);
        }else{
          _logger.log('succesful auth & callback with fb authorizeUSer in handleRegistration ',
                      {fb_response_body: response.body});

          callback(false, response.body);
        }
      });
      break;
    case 'twitter':
      var tw = new Twitter(body.provider_token, body.provider_token_secret);
      tw.authorizeUser(function(error, result){
        if(error){
          _logger.error('tw authorize error in handleRegistration: ',
                      {error: error});
          callback(error, false);
        }else{
          _logger.info('succesful auth & response with tw authorizeUser in hanldeRegistration ',
                        {tw_response_body: result});
          callback(false, result);
        }
      });
      break;
    default:
      //there is no default. therefore the requested provider authorization is 
      //not currently supported. Log & reutrn error
      _logger.log('A unsupported provider type was passed to user registration ',
                  {provider: body.provider});
      
      callback(Error.unsupportedProviderType(body.provider), false);
      break;
  }
}

/**
 * Validates the login request body has required properties to
 * process authenticating the user. This can be in the form of 
 * traditional authentication {username:pass} OR social authentication
 * which requries {provider, provider_id, & provider_token}
 * 
 * @param  {Object}  body The request body object
 * @return {Boolean}
 */
var isValidLoginRequest = function(body){
  // console.log("is valid login request: (body) " + JSON.stringify(body));
  if(body.email && body.password){
    return true;
  }else{
    if(isSocialProvider(body)){
      return true;
    }
    return false;
  }
}

/**
 * Determines if the request body is/has social provider attributes
 * 
 * @param  {Object}  body The request body object
 * @return {Boolean}
 */ 
var isSocialProvider = function(body){
  if(body.provider && body.provider_token){
    console.log('isSocialProvider -- body:' + JSON.stringify(body));
    return true;
  }
  return false;
}
