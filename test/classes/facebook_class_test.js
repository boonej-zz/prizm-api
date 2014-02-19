/**
 * Facebook Class Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose')
  , _request      = require('request')
  , _chai         = require('chai')
  , _expect       = _chai.expect
  , _should       = _chai.should()
  , _assert       = _chai.assert
  , _thisapp      = require(process.cwd() + '/server')
  , _prism_home   = process.env.PRISM_HOME
  , _config 			= require('config')
  , User 					= require(_prism_home + 'models/user').User
  , Facebook 			= require(_prism_home + 'classes/Facebook');

  describe('Facebook Class Unit Tests', function(done){
  	var test_fb_id = '100007786152724';
  	var test_fb_token = null;

  	beforeEach(function(done){
  		var fb_config = _config.social.facebook;

  		var fb_token_url = fb_config.token_uri + 'client_id='
  											+ fb_config.client_id + '&client_secret=' 
  											+ fb_config.client_secret
  											+ '&grant_type=client_credentials';

  		//fetch fb access_token
  		_request({
  			method: 'GET',
  			url: fb_token_url,
  			json: true
  		}, function(error, response){
  			if(error) throw error;
  			test_fb_token = response.body.split("=")[1];
  			done();
  		});

  	});

  	describe('Authorizing A Facebook User', function(done){
  		it('should return a facebook user object with valid id & token', function(done){
  			var fb = new Facebook(test_fb_token);
  			fb.authorizeUser(function(err, result){
  				result = result.body;
  				result.should.be.a('Object');
  				_expect(err).to.be.false;
  				_expect(result).to.have.property('name');
  				_expect(result).to.have.property('first_name');
  				_expect(result).to.have.property('last_name');
  				_expect(result).to.have.property('id');
  				_expect(result).to.have.property('gender');
  				_expect(result).to.have.property('username');
  				_expect(result).to.have.property('locale');
  				result.id.should.equal(test_fb_id);
  				done();
  			});
  		});
  		it('should return a error when given an invalid access_token and correct id', function(done){
  			var fb = new Facebook('adfnm3423mnmnfsdfafd');
  			fb.authorizeUser(function(err, result){
  				_expect(err).to.not.be.false;
  				_expect(err).to.not.be.null;
  				_expect(err).to.have.property('status_code');
  				_expect(err).to.have.property('error_info');
  				_expect(err.status_code).to.be.above(399);
  				_expect(err.error_info.error).to.equal('OAuthException');
  				_expect(err.error_info.error_description).to.equal('Invalid OAuth access token.');
  				_expect(result).to.not.be.false;
  				done();
  			});
  		});
  		//todo: should return error if user doesnt exist
  	});
  });