/**
 * Utility/Helper Methods
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _crypto   = require('crypto');
var Token     = require(process.env.PRISM_HOME + 'models/auth').Token;
var Client    = require(process.env.PRISM_HOME + 'models/auth').ClientApplication;

/**
 * String encryption method
 *
 * @param {String} string The string value to encrypt
 * @param {String} salt_key Salt key to use in creating cipher
 * @return {String} The encrypted value of string param
 */
exports.prismEncrypt = function(string, salt_key){
  var cipher  = _crypto.createCipher('aes-256-cbc', salt_key);
  var crypted = cipher.update(string, 'utf8', 'hex');
  crypted     += cipher.final('hex');
  return crypted;
};

/**
 * Standardizes HTTP Response messaging
 *
 * @param {HTTPResonse} res The response object
 * @param {Object} data The data results to be returned *can be either dictionary or array
 * @param {BOOL} success Boolean value representing a successful or failed request
 * @param {Error} error An error dictionary/hash if the the success bool is false
 * @param {Number} force_status_code Forced HTTP status to set
 */
exports.prismResponse = function(res, data, success, error, force_status_code){
  var response = {metadata: null, data: null};
  response.metadata = { success: success };
  response.data = (Array.isArray(data) ? data : [data]);

  if(error && success === false){
    response.error = error.error_info;
    response.data = [];
  }

  if(force_status_code){
    res.statusCode = force_status_code;
  }else{
    if(error && error.status_code) res.statusCode = error.status_code;
  }

  res.send(response);
};

/**
 * [parsedQueryOptions description]
 * @param  {[type]} query_params [description]
 * @return {[type]}              [description]
 */
exports.parsedQueryOptions = function(query_params) {
  var options = {};
  var obj = {};

  if(typeof(query_params) !== 'undefined'){
    options.fields = {};

    if(typeof(query_params.fields) !== 'undefined'){
      var fields = query_params.fields.split(',');
      for (var i = fields.length - 1; i >= 0; i--) {
        options.fields[fields[i]] = 1;
      }

    }else{
      query_params.fields = options.fields;
    }

    if(typeof(query_params.skip) !== 'undefined'){
      options.skip = query_params.skip;

    }else {
      options.skip = query_params.skip;
    }

    if(typeof(query_params.sort_field) !== 'undefined'){
      obj[query_params.sort_field] = -1;
      options.sort = obj;

    }else {
      obj.create_date = -1;
      options.sort = obj;
    }

    options.limit = (typeof(query_params.limit) !== 'undefined' ? query_params.limit : 30);
    return options;

  }else {
    return null;
  }
};

/**
 * [buildQueryObject description]
 * @param  {[type]}  model              [description]
 * @param  {[type]}  criteria           [description]
 * @param  {[type]}  options            [description]
 * @param  {Boolean} is_aggregate_query [description]
 * @return {[type]}                     [description]
 */
exports.buildQueryObject = function(model, criteria, options, is_aggregate_query){
  if(!is_aggregate_query){
    var query = model.find(criteria);

    if(options !== null){
      if(typeof(options.skip) !== 'undefined') query.skip(options.skip);
      if(typeof(options.sort) !== 'undefined') query.sort(options.sort);
      if(typeof(options.limit) !== 'undefined') query.limit(options.limit);
      if(typeof(options.fields) !== 'undefined') query.select(options.fields);
    }

    return query;

  }else{
    //TODO: add aggregation query builder
    return null;
  }
  return null;
};

/**
 * Explodes request path into an array
 *
 * @param {HTTPRequest} req The request object
 * @return {Array} Array with ordered paths
 */
exports.requestPathArray = function(req){
  var path = req.params;
  if(Array.isArray(path)){
    if(path.length > 0){
      return path[0].split('/');
    }
  }
  return false;
};

/**
 * Checks for Basic Authorization header & decodes credentials
 *
 * @param {String} header The Authorization header string value
 * @return {Array} Returns an array of user:pass passed by  Basic Auth headers
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
exports.authorizeClientRequest = function(req, callback){
  var creds = credentials(req.get('Authorization'));
  if(creds){
    Client.findOne({client_id: creds[0], client_secret: creds[1]}, function(error, result){
      if(error){
        callback(error, false, result);
      }else{
        callback(error, true, result);
      }
    });
  }else{
    var bearerAuth = req.get('Authorization').split(" ");
    if(bearerAuth.length == 2 && bearerAuth[0] == 'Bearer'){
      Token.findOne({access_token: bearerAuth[1]}, function(error, result){
        if(error) callback(error, false, result);
        if(result.date_expires.valueOf() > Date.now().valueOf()){
          callback(false, true, result);
        }else{
          callback(Error.accessDeniedExpiredToken, false, result);
        }
      });
    }
    callback('Error parsing credentials', false, null);
  }
};

