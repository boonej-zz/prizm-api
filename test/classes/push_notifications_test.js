/**
 * PushNotification Class Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose         = require('mongoose'),
    _request          = require('request'),
    _                 = require('underscore'),
    _chai             = require('chai'),
    _expect           = _chai.expect,
    _should           = _chai.should(),
    _assert           = _chai.assert,
    thisapp           = require(process.cwd() + '/server'),
    config            = require('config'),
    User              = require(process.env.PRISM_HOME + 'models/user').User,
    _helpers          = require(process.env.PRISM_HOME + 'test/test_helpers'),
    PushNotification  = require(process.env.PRISM_HOME + 'classes/PushNotification');

describe('PushNotification Class Unit Tests', function(done){
  var test_device = '9a830cd4 71101abf c3293c37 2248a0e5 786cca1c 5bb2d537 4e94d1d7 507c0fed';
  var mark, test_token;

  before(function(done){
    _helpers.createTestToken(function(token, code, client){
      test_token = token;
      _helpers.fetchFixtureTestUsers(function(users){
        mark = users.mark;
        mark.device_token = test_device;
        mark.save(function(err, saved){
          if(err) throw err;
          else done();
        });
      });
    });
  });

  describe('Test Push notification message sent to push server', function(done){
    it('should send object to push server', function(done){
      var activity = {
        to: mark._id.toString(),
        from: 'testthang123sedresdf',
        action: 'comment',
        post_id: 'some post id'
      };

      new PushNotification('activity', activity, function(result){
        _expect(result).to.have.property('success');
        _expect(result.success).to.equal(true);
        done();
      });
    });
  });
});
