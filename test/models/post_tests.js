/**
 * Post Model Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _request   = require('request')
  , _thisapp   = require(process.cwd() + '/server.js')
  , _mongoose  = require('mongoose')
  , _chai      = require('chai')
  , _expect    = _chai.expect
  , _should    = _chai.should()
  , _assert    = _chai.assert  
  , _logger 	 = require('winston')
  , Post 			 = require(process.env.PRISM_HOME + 'models/post').Post
  , User       = require(process.env.PRISM_HOME + 'models/user').User;

describe('Post Model Unit Tests', function(done){
	describe('Testing creating new post record', function(done){
		it('should create nested post record in user document', function(done){
			var post = new Post({
				title : 'test ttitle',
				body : 'this is the body of the test post',
				type : 'post',
				creator : {id: '123sefsdfsjkj34', name: 'Richard Tester'}
			});

			var user = new User({
				email: 'cesear@chavez.com',
				first_name: 'cesear',
				last_name: 'shavez',
				password: 'dingdong',
				posts: [post]
			});

			user.save(function(error, result){
				console.log(error);
				console.log(result);
				done();
			});
		});
	});
});