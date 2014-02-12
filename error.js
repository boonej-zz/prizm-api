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
}

exports.unauthorized = {
  status_code         : 401, 
  error_info          : {
      error             : 'unauthorized',
      error_description : 'invalid credentials'
    }
}

exports.unauthorizedClient = {
  status_code         : 401,
  error_info          : {
      error             : 'unauthorized_client',
      error_description : 'the provided client credentials are invalid'
    }
}

exports.accessDenied = {
  status_code         : 401,
  error_info          : {
      error             : 'access_denied',
      error_description : 'your credentials are not sufficient to access this' 
                             + ' resource'
    }
}

exports.unsupportedResponseType = {
  error_info          : {
      error             : 'unsupported_response_type',
      error_description : 'the specified response type is not supported by this '
                          + 'endpoint'
    }
}

exports.invalidScope = {
  error_info          : { 
      error             : 'invalid_scope',
      error_description : 'you do not have permissions to access this resource'
    }
}

exports.invalidResource = {
  status_code         : 404,
  error_info          : {
      error             : 'invalid_resource',
      error_description : 'The requested resource could not be found'
    }
}

exports.serverError = {
  status_code         : 500,
  error_info          : {
      error             : 'server_error',
      error_description : 'the server has encountered a serious error. ' 
                          + 'If this issue persists, please contact support' 
    }
}
