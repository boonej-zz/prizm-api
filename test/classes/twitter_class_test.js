/**
 * Twitter Class Unit Tests
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
  , _logger       = require('winston')
  , Error         = require(_prism_home + 'error')
  , User 					= require(_prism_home + 'models/user').User
  , Twitter 			= require(_prism_home + 'classes/Twitter');

describe('Twitter Class Unit Tests', function(done){
  var test_token        = _config.social.twitter.dev_user_access_token;
  var test_token_secret = _config.social.twitter.dev_user_access_token_secret;
  var test_user_id      = _config.social.twitter.dev_user_id;

  describe('Testing Twitter.authorizeUser method', function(done){
    it('shoud verify & authorize a valid users twitter credentials', function(done){
      var tw = new Twitter(test_token, test_token_secret);
      tw.authorizeUser(function(error, result){
        _expect(error).to.be.null;
        _expect(result).to.have.property('id');
        _expect(result).to.have.property('name');
        _expect(result).to.have.property('screen_name');
        _expect(result.id.toString()).to.equal(test_user_id);
        done();
      });
    });
    it('shoud return an error when supplied with invalid access token', function(done){
      var tw = new Twitter('asdf248990808sdfss98098sdf', test_token_secret);
      tw.authorizeUser(function(error, result){
        _logger.info('logging error returned in test: ', error);
        _expect(error).to.not.be.null;
        _expect(error.statusCode).to.equal(401);
        _expect(JSON.parse(error.data).errors[0]["code"]).to.equal(89);
        _assert(typeof(result) == 'undefined', 'result returned as an actual object.');
        done();
      });
    });
  });
});