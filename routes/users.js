/**
 * Handles routing & management for /user* endpoints
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _prism_home   = process.env.PRISM_HOME
  , _utils        = require(_prism_home + 'utils')
  , Error         = require(_prism_home + 'error')
  , User          = require(_prism_home + 'models/user').User;

exports.login = function(req, res){
  if(isValidLoginRequest(req.body)){
    if(isSocialProvider(req.body)){
      //handle social provider
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

exports.register = function(req, res){
  if(isValidRegisterRequest(req)){
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
    
    newUser.save(function(error, result){
      if(error || !result){
        _utils.prismResponse(res, null, false, Error.serverError, Error.serverError.status_code);
      }else{
        var user = result.toObject();
        delete user.password;
        delete user.comments;
        delete user.posts;
        delete user.likes;
        _utils.prismResponse(res, user, true);
      }
    })
  }else{
    _utils.prismResponse(res, null, false, Error.invalidRequest, Error.invalidRequest.status_code);
  }
}

var isValidRegisterRequest = function(req){
  console.log(req.body);


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

var hashAndValidatePassword = function(user, password_to_validate){
  //create user hash
  if(user){
    var hash_salt             = user.createUserSalt();
    var password_to_validate  = _utils.prismEncrypt(password_to_validate, hash_salt);
    if(password_to_validate == user.password) return true;
  }
  return false;
}

var isValidLoginRequest = function(body){
  if(body.email){
    if(isSocialProvider(body)){
      return true;
    }else if(body.email && body.password){
      return true;
    }
  }else{
    return false;
  }
}

var isSocialProvider = function(body){
  if(body.provider && body.provider_id && body.provider_token){
    return true;
  }
  return false;
}
