/**
 * Twitter social auth & integration
 * 
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _config 		= require('config')
	, _request 		= require('request')
	, _prism_home	= process.env.PRISM_HOME
	, _utils			= require(_prism_home + 'utils')
	, nTwitter		= require('ntwitter')
	, _logger			= require('winston')
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
function Twitter(tw_access_token, tw_access_token_secret, tw_id){
	this.tw_id										= tw_id;
	this.tw_access_token 					= tw_access_token;
	this.tw_access_token_secret 	= tw_access_token_secret;
	this.tw_client_secret					= _config.social.twitter.consumer_secret;
	this.tw_client_id							= _config.social.twitter.consumer_key;
	this.connection 							= null;
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
	if(this.tw_access_token_secret && this.tw_access_token_secret){
		this.connection = new nTwitter({
			consumer_key: 				this.tw_client_id,
			consumer_secret: 			this.tw_client_secret,
			access_token_key: 		this.tw_access_token,
			access_token_secret: 	this.tw_access_token_secret
		});

		_logger.log('verifying twitter account with access_token: ', this.tw_access_token);
		//verify user credentials
		this.connection.verifyCredentials(function(error, data){
			if(error) _logger.log('Error returned while verifying twitter user credentials: ', {error_recieved: error});
			_logger.log('Data returned from verifying twitter user credentials: ', data);
			callback(error, data);
		});
	}else{
		var invalidRequest = Error.invalidRequest;
		_logger.info('Ivalid twitter authorize user reqeust made. '
									+'missing either token or secret: ', {twitter_object: this,
																												returning_error: invalidRequest});
		callback(invalidRequest, false);
	}

};

module.exports = Twitter;