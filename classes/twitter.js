/**
 * Twitter social auth & integration
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
 * Twitter Class
 *
 * Handles Twitter user functionality & authorization
 * 
 * @param {String} tw_id The Twitter user identifier
 * @param {String} tw_access_token The Twitter user accesstoken
 */
function Twitter(tw_id, tw_access_token, tw_access_token_secret){
	this.tw_id						= tw_id;
	this.tw_access_token 	= tw_access_token;
	this.tw_access_token_secret = tw_access_token_secret;
}

/**
 * Authorizes the passed user against Twitter api
 * 
 * @param  {Function} callback The callback function to be invoked
 * @return {Error} First object sent to the callback is an error
 * 									if an error is present
 * @return {Object} Second object sent to the callback is the
 * 									response object from the twitter request 
 */
Twitter.prototype.authorizeUser = function(callback){
	
};

module.exports = Twitter;