/**
 *  Twine Class Unit Tests
 *
 * @author  DJ Hayden <dj.hayden@stablekernel.com>
 */
/*jshint -W087 */
var _thisapp    = require(process.cwd() + '/server.js'),
    _request    = require('request'),
    _mongoose   = require('mongoose'),
    _chai       = require('chai'),
    _expect     = _chai.expect,
    _should     = _chai.should(),
    _assert     = _chai.assert,
    _logger     = require('winston'),
    _helpers    = require(process.env.PRISM_HOME + 'test/test_helpers'),
    Twine       = require(process.env.PRISM_HOME + 'classes/Twine'),
    Post        = require(process.env.PRISM_HOME + 'models/post').Post,
    User        = require(process.env.PRISM_HOME + 'models/user').User,
    Activity    = require(process.env.PRISM_HOME + 'models/activity').Activity,
    Trust       = require(process.env.PRISM_HOME + 'models/user').Trust;

describe('Twine Class Unit Tests', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin, DJ;
  var follower = null;
  var followee = null;
  var follow_result = { followee: null, follower: null};
  var request_result = null;
  var test_user = null;
  var test_token = null;
  var test_client = null;
  var test_code = null;
  var test_post1 = null;
  var test_post2 = null;
  var test_post3 = null;
  var test_post4 = null;
  var test_post5 = null;
  var test_post6 = null;
  var test_post7 = null;
  var test_post8 = null;
  var test_comment_id = null;
  var test_comment_id2 = null;

  var createXArguments = function(body){
    body = JSON.stringify(body);
    return new Buffer(body).toString('base64');
  };

  var digestXArguments = function(body){
    return new Buffer(body, 'base64').toString('utf8');
  };

  before(function(done){
      _helpers.createTestUser(function(testuser){
        test_user = testuser;
        _helpers.createTestToken(function(token, code, client){
          test_token = token;
          test_code = code;
          test_client = client;
          _helpers.fetchFixtureTestUsers(function(users){
            mark = users.mark;
            edwardo = users.edwardo;
            cameron = users.cameron;
            erica = users.erica;
            sean = users.sean;
            maryolin = users.maryolin;
            DJ = users.DJ;
            _helpers.fetchFixturePosts(function(posts){
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
  //TODO: filter tests
  //TODO: paging tests
  //TODO: sorting tests
  //TODO: limit tests
  //TODO: contains tests
  //TODO: resolve
    //TODO: recursive resolve
    //TODO: resolve contains
  describe('Testing Resolve', function(done){
    before(function(done){
      User.findOne({_id: mark._id}, function(err, result){
        result.following = [
          {"_id": sean._id, "create_date": new Date()},
          {"_id": cameron._id, "date": new Date()},
          {"_id": cameron._id, "date": new Date()}
        ];
        result.save(function(){
          done();
        });
      });
    });
    it('request should resolve the following & followers fields', function(done){
      var req = {
        method: 'GET',
        url: 'https://localhost:3000/users/'+mark._id/+'following',
        strictSSL: false,
        json: true,
        body: {
          first_name: "mark",
          status: "0",
          sort: "1",
          sort_by: "create_date",
          page_by: 'following.create_date',
          page: '2015/20/12',
          page_direction: 0,
          resolve: {
            following: {
              format:"short",
              fields: ["create_date", "test"],
              contains: {
                followers: DJ._id.toString()
              }
            },
            followers: {
              format: "short",
            }
          },
          contains: {
            following: {
              _id: sean._id.toString()
            }
          }
        }
      };
      var b = JSON.stringify(req.body);
      // b = JSON.parse(b);
      var t = new Buffer(b).toString('base64');
      req.headers = {"x-arguments": t.toString()};
      new Twine('User', {_id: mark._id.toString()}, req, null, function(err, result){
        //TODO: finish tests & use cases
        debugger;
        done();
      });
    });
  });
});





















