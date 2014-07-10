/**
 * Twine Contains Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */

/*jshint -W087 */

var app       = require(process.cwd() + '/server.js'),
    home      = process.env.PRISM_HOME,
    mongoose  = require('mongoose'),
    _         = require('underscore'),
    chai      = require('chai'),
    expect    = chai.expect,
    should    = chai.should(),
    assert    = chai.assert,
    helpers   = require(home + 'test/test_helpers'),
    User      = require(home + 'models/user').User,
    Post      = require(home + 'models/post').Post,
    Trust     = require(home + 'models/trust').Trust,
    Twine     = require(home + 'classes/Twine');

describe('Twine Contains', function(done) {
  var mark, edwardo, cameron, erica, sean, maryolin, DJ;
  var p1, p2, p3, p4, p5, p6, p7, p8, p9, p10;
  var xargs, req, error, result;

  before(function(done) {
    helpers.fetchFixtureTestUsers(function(users) {
      mark    = users.mark;
      edwardo = users.edwardo;
      cameron = users.cameron;
      erica   = users.erica;
      sean    = users.sean;
      maryolin  = users.maryolin;
      DJ        = users.DJ;
      done();
    });
  });

  describe('Simple Field Contains', function(done) {
    before(function(done) {
      xargs = {contains: {followers: {_id: DJ._id.toString()}}};
      req = helpers.basicRequestObject(xargs);
      new Twine('User', {_id: mark._id}, req, {}, function(err, res) {
        result = res;
        error = err;
        done();
      });
    });

    it('should not return an error', function(done) {
      assert.isFalse(error);
      done();
    });

    it('should return only the passed objectId in specified field', function(done) {
      var item = result.data[0];
      var followers = item.followers;
      assert.lengthOf(followers, 1);
      assert.equal(followers[0]._id, DJ._id.toString());
      done();
    });

    it('should only return a data property in results', function(done) {
      assert.lengthOf(_.keys(result), 1);
      done();
    });

    it('should not return field if doesnt contain passed value', function(done) {
      var x = {contains: {followers: {_id: 'asdfasd234sdsdf234'}}};
      var r = helpers.basicRequestObject(r);
      new Twine('User', {_id: mark._id}, r, null, function(err, res) {
        var item = res.data[0];
        assert.notProperty(item, 'followers');
        done();
      });
    });
  });

  describe('Multiple Field Contains', function(done) {
    var testuser;
    
    before(function(done) {
      testuser  = mark._id.toString();
      xargs     = {contains: {followers: {_id: testuser}, following: {_id: testuser}}};
      req       = helpers.basicRequestObject(xargs);
      new Twine('User', {_id: DJ._id}, req, null, function(err, res){
        error = err;
        result = res;
        done();
      });
    });

    it('should not return an error', function(done) {
      assert.isFalse(error);
      done();
    });

    it('should return only the passed objectId in the first specified field', function(done) {
      var item = result.data[0];
      var followers = item.followers;
      var following = item.following;
      assert.lengthOf(followers, 1);
      assert.equal(followers[0]._id, testuser);
      assert.equal(following[0]._id, testuser);
      done();
    });
  });
});
