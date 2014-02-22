/**
 * User Route Unit Tests
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

describe('User Route Unit Tests', function(done){
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
  
  afterEach(function(done){
    _t_helpers.destroyTestUser(function(){
      _t_helpers.destroyTestToken(function(){
        _t_helpers.destroyTestCode(function(){
          _t_helpers.destroyTestClient(function(){
            done();
          });
        });
      });
    })
  });

  describe('Testing User Login', function(done){
    it('should properly login an existing user with basic auth', function(done){
      var fetch_url = 'https://localhost:3000/oauth2/login';
      var auth_header = _t_helpers.fetchAuthHeader( testClient.client_id, 
                                                    testClient.client_secret);
      var request_body = {
        email: testUser.email,
        password: 'testpassword'
      };
      _request({
        method: 'POST',
        url: fetch_url,
        json: true,
        strictSSL: false,
        body: request_body,
        headers: {'Authorization' : auth_header}
      }, function(error, result, body){
        body = JSON.stringify(body);
        var login = JSON.parse(body);
        // console.log(login);
        _expect(login).to.have.property('metadata');
        _expect(login).to.have.property('data');
        _expect(login.data[0].email).to.equal(testUser.email);
        _expect(login.metadata.success).to.equal(true);
        done();
      });
    });
    it('should return error if user does not exist', function(done){
      var auth_header = _t_helpers.fetchAuthHeader( testClient.client_id, 
                                                    testClient.client_secret);
      var fetch_url = 'https://localhost:3000/oauth2/login';
      var request_body = {
        email: 'dabears@chicago.com',
        password: 'dabears'
      };
      _request({
        method: 'POST',
        url: fetch_url,
        json: true,
        strictSSL: false,
        body: request_body,
        headers: {'Authorization' : auth_header}
      }, function(error, result, body){
        body = JSON.stringify(body);
        var fail = JSON.parse(body);
        _expect(fail).to.have.property('metadata');
        _expect(fail).to.have.property('error');
        done();
      });
    });
  });
  describe('Testing User Registration', function(done){
    it('should register a user with valid post body && access_token', function(done){
      _t_helpers.destroyTestUser(function(){
        var auth_header = "Bearer " + testToken.access_token;
        var fetch_url = 'https://localhost:3000/users';
        var request_body = {
          first_name: 'Dj',
          last_name: 'Hayden',
          email: 'dj.awesome2@test.com',
          gender: 'Male',
          zip_postal: '98122',
          city: 'Seattle',
          state: 'Washington',
          birthday: '1980-12-01',
          password: 'testregister'
        };
        _request({
          method: 'POST',
          url: fetch_url,
          json: true,
          strictSSL: false,
          body: request_body,
          headers: { "Authorization" : auth_header }
        }, function(error, result, body){
          // console.log(JSON.stringify(body));
          done();
        });
      });
    });
  });
  describe('Testing Adding Post to a User {POST} /users/:id/posts', function(done){
    it('should allow you to create a post', function(done){
      _t_helpers.createTestUser(function(user){
        if(user){
          testUser = user;

          var fetch_url = 'https://localhost:3000/users/'
                          + testUser._id + '/posts';
          var auth_header = 'Bearer ' + testToken.access_token;
          _request({
            method: 'POST',
            url: fetch_url,
            json: true,
            strictSSL: false,
            headers: {"Authorization" : auth_header},
            body: {text: 'this is a test post', creator: testUser._id}
          }, function(error, post){
            console.log(post.body);
            done();
          });
        }else{
          _expect(false).to.be.true;
          done();
        }
      });
    });
    it('should allow you to page posts', function(done){
      _t_helpers.createTestUser(function(user){
        if(user){
          testUser = user;
          user.posts.push(new Post({text: 'test test tes',  type: 'posts', creator: testUser._id}));
          user.posts.push(new Post({text: 'test test tes2', type: 'posts', creator: testUser._id, create_date: Date.now() + 10 * 60 * 100}));
          user.posts.push(new Post({text: 'test test tes3', type: 'posts',creator: testUser._id, create_date: Date.now() - 10 * 60 * 100}));
          user.posts.push(new Post({text: 'test test tes3', type: 'posts', creator: testUser._id, create_date: Date.now() - 5 * 60 * 100}));
          user.posts.push(new Post({text: 'test test tes3', type: 'posts',creator: testUser._id, create_date: Date.now() + 20 * 60 * 100}));
          user.posts.push(new Post({text: 'test test tes3', type: 'posts',creator: testUser._id, create_date: Date.now() + 30 * 60 * 100}));
          user.posts.push(new Post({text: 'test test tes3', type: 'posts',creator: testUser._id, create_date: Date.now() + 2 * 60 * 60 * 1000}));
          user.posts.push(new Post({text: 'test test tes3', type: 'posts',creator: testUser._id, create_date: new Date() + 60 * 60 * 100}));
          user.save(function(error, result){
            if(error){
              _expect(false).to.be.true;
              done();
            }else{

              // User.aggregate({$project: { "posts":1}}, 
              //   {$match: {"posts.create_date" : {$gte: (new Date())}}}, 
              //   {$unwind: "$posts"}, 
              //   {$sort: {"posts.create_date": -1}}, 
              //   {$limit: 4},
              //   function(error, result){
              //     console.log(result);
              //     done();
              //   });
              var fetch_url = 'https://localhost:3000/users/'+user._id+'/posts?count=4&feature_identifier='+Date.now()

              var auth_header = 'Bearer ' + testToken.access_token;
              _request({
                method: 'GET',
                url: fetch_url,
                json: true,
                strictSSL: false,
                headers: {"Authorization" : auth_header}
                
              }, function(error, post){
                console.log(post.body);
                done();
              });
            }
          });
        }else{
          done();
        }
      });
    });
  });
  describe('Testing Fetching User', function(done){
    it('should retrieve a user with a valid identifier', function(done){
      _t_helpers.createTestUser(function(user){
        if(user){
          testUser = user;

          var fetch_url = 'https://localhost:3000/users/' + testUser._id;
          var auth_header = 'Bearer ' + testToken.access_token; 
          _request({
            method: 'GET',
            url: fetch_url,
            json: true,
            strictSSL: false,
            headers: {"Authorization" : auth_header}
          }, function(error, result, body){
            // console.log(result.body);
            _expect(result.body.data[0]).to.have.property('create_date');
            _expect(result.body.data[0]).to.have.property('first_name');
            _expect(result.body.data[0]).to.have.property('last_name');
            _expect(result.body.data[0]).to.have.property('email');
            _expect(result.body.data[0]).to.have.property('_id');
            _expect(result.body.data[0]._id).to.equal(testUser._id.toString());
            done();
          });
        }else{
          _expect(false).to.be.true;
          done();
        }
      });
    });  
  });
});



