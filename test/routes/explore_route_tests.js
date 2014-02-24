/**
 * Explore Route Unit Tests
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
  , _auth_model   = require(_prism_home + 'models/auth')
  , _user_route   = require(_prism_home + 'routes/users')
  , _t_helpers    = require(_prism_home + 'test/test_helpers')
  , Post          = require(_prism_home + 'models/post').Post
  , User          = require(_prism_home + 'models/user').User;

describe('Explore Route Unit Tests', function(done){
  var test_date = Date.now() + 9 * 60 * 60 * 1000;
  test_date = new Date(test_date);
  var fetch_url = 'https://localhost:3000/explore?feature_identifier=' + test_date.toISOString();
  var testUser = null;
  var testClient = null;
  var testCode = null;
  var testToken = null;


  beforeEach(function(done){
    _t_helpers.createTestUser(function(user){
      testUser = user;
      _t_helpers.createTestToken(function(token, code, client){
        testToken = token;
        testCode = code;
        testClient = client;
        done();
      });
    });
  });

  describe('Testing /explore fetching all posts for explore', function(done){

    it('should return all public posts sorted by a desc create_date', function(done){
      var auth_headers = 'Bearer ' + testToken.access_token;
      var posts = _t_helpers.fetchFakePostsArray(testUser, testUser);
      Post.create(posts, function(error, postsCreated){
        if(error) throw error;

        _request({
          method: 'GET',
          url: fetch_url,
          json: true,
          strictSSL: false,
          headers: {"Authorization":auth_headers}
        }, function(err, expl){
          console.log(expl.body);
          done();
        });
      });

    });
  });
});
