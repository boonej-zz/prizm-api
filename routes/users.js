/**
 * Handles routing & management for /user* endpoints
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _prism_home   = process.env.PRISM_HOME
  , _utils        = require(_prism_home + 'utils')
  , Error         = require(_prism_home + 'error')
  , Facebook      = require(_prism_home + 'classes/Facebook')
  , User          = require(_prism_home + 'models/user').User;

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
        if(error){
          //social login failure - user does not exist or 
          //failire to authenticate with social provider
          _utils.prismResponse( res,
                                null,
                                false,
                                error,
                                error.status_code);
        }else{
          //succesful login - send back returned user object
          _utils.prismResponse( res, result, true);
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
    
    //check, validate, & handle social registration
    if(isSocialProvider(req.body)){
      if(isValidSocialRegisterRequest(req)){
        //valid request, add addition social fields to userobject
        newUser.provider = req.body.provider;
        newUser.provider_id = req.body.provider_id;
        newUser.provider_token = req.body.provider_token;
        if(newUser.provider == 'twitter'){
          newUser.provider_token_secret = req.body.provider_token_secret;
        }
      }else{
        //not a valid request, return Error
        _utils.prismResponse(res, null, false, Error.invalidRequest, Error.status_code);
      }
    }

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
        delete user.comments;
        delete user.posts;
        delete user.likes;
        _utils.prismResponse(res, user, true);
      }
    });
    
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
        _utils.prismResponse(res, result, true);
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
      typeof(req.body.email) == 'undefined'     ||
      typeof(req.body.gender) == 'undefined' ||
      typeof(req.body.zip_postal) == 'undefined' ||
      typeof(req.body.city) == 'undefined' ||
      typeof(req.body.state) == 'undefined' ||
      typeof(req.body.birthday) == 'undefined' ){
  
    return false;

  }else{
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
        typeof(req.body.provider_token) == 'undefined' ||
        typeof(req.body.provider_id) == 'undefined'){
      return false;
    }else{
      if(req.body.provider == 'twitter'){
        if(typeof(req.body.provider) == 'undefined') return false;
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
      var fb = new Facebook(body.provider_id, body.provider_token);
      fb.authorizeUser(function(error, response){
        if(error) callback(error, response.body);
        if(response){
          fb.isPrismUser(function(error, response){
            if(error){
              //send back error that user does not exist in prism db
              callback(Error.invalidSocialUser, false);
            }else{
              //return succesful object & invoke callback
              callback(false, response);
            }
          });
        }
      });
      break;
    case 'twitter':
      //twitter logic
      break;
    case 'google':
      //google logic
      //for the meantime send back as unsupported
      //TODO: send back
      callback(Error.invalidRequest, false);
      break;
    default:
      //send error back unsupported provider.
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
  if(body.provider && body.provider_id && body.provider_token){
    return true;
  }
  return false;
}
