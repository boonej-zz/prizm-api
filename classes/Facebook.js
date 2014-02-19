/**
 * Facebook social auth & integration
 * 
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _config 		= require('config')
	, _request 		= require('request')
	, _prism_home	= process.env.PRISM_HOME
	, _utils			= require(_prism_home + 'utils')
	, Error 			= require(_prism_home + 'error')
	, User 				= require(_prism_home + 'models/user');

/**
 * Facebook Class
 *
 * Handles facebook user functionality & authorization
 * 
 * @param {String} fb_id The facebook user identifier
 * @param {String} fb_access_token The facebook user accesstoken
 */
function Facebook(fb_id, fb_access_token){
	this.fb_id 						= fb_id;
	this.fb_access_token 	= fb_access_token;
	this.fb_profile				= null;
}

/**
 * Authorizes the passed user against facebook graph
 * 
 * @param  {Function} callback The callback function to be invoked
 * @return {Error} First object sent to the callback is an error
 * 									if an error is present
 * @return {Object} Second object sent to the callback is the
 * 									response object from the fb request 
 */
Facebook.prototype.authorizeUser = function(callback){
	if(this.fb_access_token){
		//construct request url
		var url = _config.social.facebook.base_uri + '/me?access_token=' + this.fb_access_token;

		//construct & execute fb user request					
		_request({
			method: 'GET',
			url: url,
			json: true
		}, function(error, response){
			console.log(response.body);
			if(error){
				console.log(error);
				callback(error, false);
			}else{
				if(response.statusCode >= 400){
					var fb_auth_error = {
						status_code	: response.statusCode,
						error_info : {
							error: response.body.error.type,
							error_description: response.body.error.message
						}
					};
					callback(fb_auth_error, response);
				}else{
					this.fb_id = response.body.id;
					this.fb_profile = response.body;
					callback(false, response);
				}
			}
		});
	}else{
		//invoke callback with error, required properties were not passed
		callback(Error.invalidFacebookAuth, false);
	}
};

/**
 * Checks to see if the validated facebook user id is associated to
 * a prism user object
 * 
 * @param  {Function} callback The callback block to be invoked
 * @return {Error} The error object if produced
 * @return {Object} The located existing user object
 */
Facebook.prototype.isPrismUser = function(callback){
	if(this.fb_id){
		User.findUserByFacebookId(function(error, result){
			console.log(error);
			console.log(result);
			if(error){
				console.log("Fetching Facebook user by id failed with error: " + error);
				callback(error, false);
			}else if(result._id){
				callback(false, result);
			}else{
				console.log("no error OR facebook user object returned.. issue server error");
				callback(Error.serverError, false);
				throw 'No provider user found or error returned! SERVER EXCEPTION';
			}
		});
	}else{
		//send error user needs to be authorized first
		callback(Error.invalideFacebookAuth, false);
	}
};

/**
 * Fetchs registered application auth code & acess_token
 * @param  {Function} callback
 * @return {[type]}
 */
Facebook.prototype.fetchAccessToken = function(callback){};

module.exports = Facebook;
