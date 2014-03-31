/**
 * User Route Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _request      = require('request'),
    _chai         = require('chai'),
    _expect       = _chai.expect,
    _should       = _chai.should(),
    _assert       = _chai.assert,
    _thisapp      = require(process.cwd() + '/server'),
    _prism_home   = process.env.PRISM_HOME,
    _auth_model   = require(_prism_home + 'models/auth'),
    _user_route   = require(_prism_home + 'routes/users'),
    _t_helpers    = require(_prism_home + 'test/test_helpers'),
    Post          = require(_prism_home + 'models/post').Post,
    User          = require(_prism_home + 'models/user').User;

describe('User Route Unit Tests', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin, DJ;
  var testUser = null;
  var testClient = null;
  var testCode = null;
  var testToken = null;
  var test_post1, test_post2, test_post3, test_post4, test_post5, test_post6, test_post7, test_post8;

  before(function(done){
    _t_helpers.createTestUser(function(user){
      testUser = user;
      _t_helpers.createTestToken(function(token, code, client){
        testToken = token;
        testCode = code;
        testClient = client;
        _t_helpers.fetchFixtureTestUsers(function(users){
          mark = users.mark;
          edwardo = users.edwardo;
          cameron = users.cameron;
          erica = users.erica;
          sean = users.sean;
          maryolin = users.maryolin;
          DJ = users.DJ;
          _t_helpers.fetchFixturePosts(function(posts){
            test_post1 = posts[0];
            test_post2 = posts[1];
            test_post3 = posts[2];
            test_post4 = posts[3];
            test_post5 = posts[4];
            test_post6 = posts[5];
            test_post7 = posts[6];
            test_post8 = posts[7];
            done();
          });
        });
      });
    });
  });

  //TODO: need to update this test..
   describe.skip('Testing User Login', function(done){
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
      // _t_helpers.destroyTestUser(function(){
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
      // });
    });
  });
  describe('Testing Adding Post to a User {POST} /users/:id/posts', function(done){
    it('should allow you to create a post', function(done){
      testUser = DJ;

      var fetch_url = 'https://localhost:3000/users/' + testUser._id + '/posts';
      var auth_header = 'Bearer ' + testToken.access_token;
      _request({
        method: 'POST',
        url: fetch_url,
        json: true,
        strictSSL: false,
        headers: {"Authorization" : auth_header},
        body: {
          text: 'this is a test post',
          creator: testUser._id,
          category: 'experiences'

        }
      }, function(error, post){
        _expect(post.body.data[0]).to.have.property('_id');
        _expect(post.body.data[0].target_id).to.equal(testUser._id.toString());
        _expect(post.body.data[0].creator._id).to.equal(testUser._id.toString());
        done();
      });
    });
    it('should save your location details when creating a post', function(done){
      testUser = DJ;
      var location_name = 'Location Test';
      var location_longitude = 34.3434;
      var location_latitude = 3.12312323;
      var fetch_url = 'https://localhost:3000/users/' + testUser._id + '/posts';
      var auth_header = 'Bearer ' + testToken.access_token;
      _request({
        method: 'POST',
        url: fetch_url,
        json: true,
        strictSSL: false,
        headers: {"Authorization" : auth_header},
        body: {
                text: 'test post with location',
                creator: testUser._id,
                location_name: location_name,
                location_latitude: location_latitude,
                location_longitude: location_longitude,
                category: 'experiences'
              }
      }, function(error, result){
        _expect(result.body.data[0]).to.have.property('location_name');
        _expect(result.body.data[0]).to.have.property('location_longitude');
        _expect(result.body.data[0]).to.have.property('location_latitude');
        _expect(result.body.data[0].location_name).to.equal('Location Test');
        _expect(result.body.data[0].location_latitude).to.be.a('Number');
        _expect(result.body.data[0].location_longitude).to.be.a('Number');
        _expect(result.body.data[0].location_latitude).to.equal(3.12312323);
        _expect(result.body.data[0].location_longitude).to.equal(34.3434);
        done();
      });
    });
    it('should update the users post_count after they create a post', function(done){
      var user = cameron;
      var before_p = user.posts_count;

      var fetch_url = 'https://localhost:3000/users/'+user._id+'/posts';
      _request({
        method: 'POST',
        url: fetch_url,
        headers: {"Authorization" : 'Bearer ' + testToken.access_token},
        json: true,
        strictSSL: false,
        body:{
          text: 'im posting this test to make sure i see my posts increase',
          creator: user._id,
          category: 'experiences'
        }
      }, function(err, result){
        _expect(result.body.metadata.success).to.equal(true);

        User.findOne({_id: user._id}, function(err, u_post){
          _expect(u_post.posts_count).to.be.above(before_p);
          done();
        });
      });
    });
    //TODO: fix & update this test
    it.skip('should allow you to page posts', function(done){
        testUser = mark;
        var fi = new Date();
        var fetch_url = 'https://localhost:3000/users/'+testUser._id+'/posts?limit=5&feature_identifier='+fi.toISOString();
        var auth_header = 'Bearer ' + testToken.access_token;
        _request({
          method: 'GET',
          url: fetch_url,
          json: true,
          strictSSL: false,
          headers: {"Authorization" : auth_header}
        }, function(error, post){
          _expect(error).to.equal(null);
          _expect(post.body.data).to.have.length(5);
          post.body.data.forEach(function(apost){
            _expect(new Date(apost.create_date).valueOf()).to.be.above(fi.valueOf());
          });
          done();
        });
    });
  });
  //TODO: fix & update this test
  describe.skip('Testing Reposting a User Post', function(done){
    var test_user;
    var test_repost_user = null;
    var test_repost_origin = null;
    var test_repost_result = null;
    before(function(done){
      //create test user
      _t_helpers.createTestUser(function(user){
        test_user = user;
        //create repost user
        _t_helpers.createTestUser(function(second_user){
          test_repost_user = second_user;
          //create origin post
          var origin_post = new Post({
            text: 'origin post playa!!',
            creator: user._id,
            category: 'personal',
            target_id: user._id
          }).save(function(err, org_post){
            if(err) throw err;
            test_repost_origin = org_post;
            //request to create repost
            _request({
              method: 'POST',
              headers: {'Authorization':'Bearer '+ testToken.access_token},
              url: 'https://localhost:3000/users/'+second_user._id+'/posts',
              strictSSL:false,
              json:true,
              body:{
                text: org_post.text,
                creator: second_user._id,
                category: org_post.category,
                target_id: second_user._id,
                origin_post_id: org_post._id
              }
            }, function(err, result){
              if(err) throw err;
              test_repost_result = result.body;
              done();
            });
          });
        });
      });
    });
    it('should set the origin_post_id with the origin post_id', function(done){
      _expect(test_repost_result.data[0].origin_post_id.toString()).to.equal(test_repost_origin._id.toString());
      done();
    });
    it('should set the is_repost property to true', function(done){
      _expect(test_repost_result.data[0].is_repost).to.equal(true);
      done();
    });
    it('should set the origin post creator short user object in the post return', function(done){
      _expect(test_repost_result.data[0]).to.have.property('origin_post_creator');
      var post_creator = test_repost_result.data[0].origin_post_creator;
      _expect(post_creator).to.have.property('_id');
      _expect(post_creator).to.have.property('name');
      _expect(post_creator).to.have.property('first_name');
      _expect(post_creator).to.have.property('last_name');
      _expect(post_creator).to.have.property('profile_photo_url');
      _expect(post_creator._id.toString()).to.equal(test_user._id.toString());
      done();
    });
  });
  describe('Testing Fetching User', function(done){
    it('should retrieve a user with a valid identifier', function(done){
          var fetch_url = 'https://localhost:3000/users/' + DJ._id.toString();
          var auth_header = 'Bearer ' + testToken.access_token;
          _request({
            method: 'GET',
            url: fetch_url,
            json: true,
            strictSSL: false,
            headers: {"Authorization" : auth_header}
          }, function(error, result, body){
            _expect(result.body.data[0]).to.have.property('create_date');
            _expect(result.body.data[0]).to.have.property('first_name');
            _expect(result.body.data[0]).to.have.property('last_name');
            _expect(result.body.data[0]).to.have.property('email');
            _expect(result.body.data[0]).to.have.property('_id');
            _expect(result.body.data[0]._id).to.equal(DJ._id.toString());
            done();
          });
        });
    it('should resolve the following array when specified in body', function(done){
      _request({
        method: 'GET',
        url: 'https://localhost:3000/users/'+DJ._id.toString(),
        json: true,
        strictSSL: false,
        headers: {"Authorization":"Bearer "+ testToken.access_token},
        body: {'resolve': {following: {format:'short'}}}
      }, function(err, result){
        _expect(result.body).to.have.property('metadata');
        _expect(result.body).to.have.property('resolve');
        _expect(result.body).to.have.property('data');
        var following_count = result.body.data[0].following.length;
        _expect(result.body.resolve).to.have.property('following');
        _expect(result.body.resolve.following.length).to.equal(following_count);
        done();
      });
    });
    it('should return & resolve object in following if contains is set as well' , function(done){
      _request({
        method: 'GET',
        url: 'https://localhost:3000/users/'+DJ._id.toString(),
        json: true,
        strictSSL: false,
        headers: {"Authorization":"Bearer "+testToken.access_token},
        body: {'resolve': {following: {format:'short'}}, 'contains':{'following': {_id: mark._id.toString()}}}
      }, function(err, result){
        _expect(result.body.data[0].following.length).to.equal(1);
        _expect(result.body.data[0].following[0]._id.toString()).to.equal(mark._id.toString());
        done();
      });
    });
  });
  describe('Testing Searching for A User', function(done){
    it('should fetch a user by there first name', function(done){
      var fetch_url = 'https://localhost:3000/users?last_name=zuck';
      var auth_header = 'Bearer ' + testToken.access_token;
      _request({
        method: 'GET',
        url: fetch_url,
        json: true,
        strictSSL: false,
        headers: {"Authorization" : auth_header}
      }, function(error, result, body){
        // console.log(result.body);
        _expect(result.body.data[0]).to.have.property('name');
        _expect(result.body.data[0]).to.have.property('first_name');
        _expect(result.body.data[0]).to.have.property('last_name');
        _expect(result.body.data[0]).to.have.property('_id');
        _expect(result.body.data[0].first_name).to.equal('mark');
        done();
      });
    });
  });
});












