/**
 * Error types & descriptors
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */

//TODO: this needs to be turned into a class object that
//handles error management & formats return types properly.
//ie. move the auth error methods into this class & handle
//invoking the reponse object & redirect

exports.invalidRequest = {
  status_code         : 400,
  error_info          : {
    error             : 'invalid_request',
    error_description : 'the request was malformed or missing required data'
  }
};

exports.invalidLoginRequest = {
  status_code         : 400,
  error_info          : {
    error             : 'invalid_login_request',
    error_description : 'the login request was attempted without an email address'
  }
};

exports.invalidLoginUserDoesNotExist = {
  status_code         : 400,
  error_info          : {
    error             : 'user_does_not_exist',
    error_description : 'no matching user email found'
  }
};

exports.invalidRegisterUserExists = {
  status_code         : 400,
  error_info          : {
    error             : 'invalid_registration',
    error_description : 'invalid registration, user already exists'
  }
};

exports.invalidUserCredentials = {
  status_code         : 401,
  error_info          : {
    error             : 'invalid_user_credentials',
    error_description : 'User email/password does not match'
  }
};

exports.invalidUserRequest = {
  status_code         : 400,
  error_info          : {
    error             : 'invalid_user_request',
    error_description : 'a valid user identifier is required to request a user'
  }
};

exports.unauthorized = {
  status_code         : 401,
  error_info          : {
      error             : 'unauthorized',
      error_description : 'invalid credentials'
    }
};

exports.invalidSocialUser = {
  status_code         : 400,
  error_info          : {
    error             : 'user_does_not_exist',
    error_description : 'the socially authenticated user does not exist ' +
                         'in Prism, user registration required'
  }
};

exports.unauthorizedClient = {
  status_code         : 401,
  error_info          : {
      error             : 'unauthorized_client',
      error_description : 'the provided client credentials are invalid'
    }
};

exports.accessDenied = {
  status_code         : 401,
  error_info          : {
      error             : 'access_denied',
      error_description : 'your credentials are not sufficient to access this'+
                          ' resource'
    }
};

exports.accessDeniedExpiredToken = {
  status_code         : 401,
  error_info          : {
    error             : 'access_denied',
    error_description : 'access_token has expired. please re-authenticate'
  }
};

exports.unsupportedResponseType = {
  status_code           : 400,
  error_info            : {
      error             : 'unsupported_response_type',
      error_description : 'the specified response type is not supported by this '+
                          'endpoint'
    }
};

exports.unsupportedProviderType = function(type){
  var unsuportedProvider = {
    status_code         : 400,
    error_info          : {
      error             : 'unsupoprted_privder_type',
      error_description : type + 'is not a supported provider type'
    }
  };
  return unsupportedProvider;
};

exports.invalidScope = {
  status_code           : 403,
  error_info            : {
      error             : 'invalid_scope',
      error_description : 'you do not have permissions to access this resource'
    }
};

exports.invalidResource = {
  status_code         : 404,
  error_info          : {
      error             : 'invalid_resource',
      error_description : 'The requested resource could not be found'
    }
};

exports.serverError = {
  status_code         : 500,
  error_info          : {
      error             : 'server_error',
      error_description : 'the server has encountered a serious error. '+
                          'If this issue persists, please contact support'
    }
};

exports.invalidFacebookAuth = {
  status_code         : 401,
  error_info          : {
    error             : 'invalid_facebook_authorization',
    error_description : 'facebook user authorization requires both valid '+
                        ' facebook user identifier & granted user access_token'
  }
};

exports.invalidTwitterAuth = {
  status_code         : 401,
  error_info          : {
    error             : 'invalid_twitter_authorization',
    error_description : 'twitter user authorization requires both valid '+
                        'access_token & access_token_secret'
  }
};


