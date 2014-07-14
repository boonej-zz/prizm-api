/**
 * Twine Resolve Tests
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

describe('Twine Resolve', function(done) {
  var mark, edwardo, cameron, erica, sean, maryolin, DJ;
  var p1, p2, p3, p4, p5, p6, p7, p8, p9, p10;

  before(function(done) {
    helpers.fetchFixtureTestUsers(function(users) {
      mark      = users.mark;
      edwardo   = users.edwardo;
      cameron   = users.cameron;
      maryolin  = users.maryolin;
      erica     = users.erica;
      sean      = users.sean;
      DJ        = users.DJ;
      done();
    });
  });


  describe('Simple Field Resolve', function(done) {
    var xargs = {resolve : {following : { format: "short" } } };
    var req = helpers.basicRequestObject(xargs);
    var result;
    var error;

    before(function(done) {
      new Twine('User', {_id: mark._id}, req, null, function(err, res) {
        result = res;
        error = err;
        done();
      });
    });

    it('should not return an error', function(done) {
      assert.isFalse(error);
      done();
    });

    it('should have a data property in result', function(done) {
      assert.property(result, 'data');
      done();
    });

    it('should have a resolve property', function(done) {
      assert.property(result, 'resolve');
      done();
    });

    it('should have a User property in resolve object', function(done) {
      assert.property(result.resolve, 'User');
      done();
    });

    it('should ensure the User property is a object', function(done) {
      assert.isObject(result.resolve.User);
      done();
    });

    it('should have ObjectId as key of resolved field under User Object in result', function(done) {
      var key_to_verify = _.keys(result.resolve.User)[0];
      assert.isTrue(helpers.isHexObjectId(key_to_verify));
      done();
    });

    it('should have a User Object value for ObjectId key in resolve.User.ObjectId', function(done) {
      var key = _.keys(result.resolve.User)[0];
      assert.isObject(result.resolve.User[key]);
      done();
    });

    it('should have a short formatted resolved object', function(done) {
      var key               = _.keys(result.resolve.User)[0];
      var resolved_object   = result.resolve.User[key];
      var num_of_properties = _.keys(resolved_object).length;
      assert.isTrue((num_of_properties < 10));
      done();
    });
  });

  describe('Ammend Return Format with Additional Fields', function(done) {
    var xargs = {resolve: {following: { format: "short", fields: ["create_date", "modify_date", "test"]}}};
    var req   = helpers.basicRequestObject(xargs);
    var result;
    var error;
    var ob_with_fields;

    before(function(done) {
      new Twine('User', {_id: mark._id}, req, null, function(err, res) {
        result = res;
        error = err;
        var key = _.keys(result.resolve.User)[0];
        ob_with_fields = result.resolve.User[key];
        done();
      });
    });

    it('should not return an error', function(done) {
      assert.isFalse(error);
      done();
    });

    it('should have a resolve object with a User object inside', function(done) {
      assert.property(result, 'resolve');
      assert.property(result.resolve, 'User');
      done();
    });

    it('should have 2 additional valid fields', function(done) {
      assert.property(ob_with_fields, 'create_date');
      assert.property(ob_with_fields, 'modify_date');
      done();
    });

    it('should not include the additional field that doesnt exist in the model', function(done) {
      assert.notProperty(ob_with_fields, 'test');
      done();
    });
  });

  describe('Multiple Field Resolve', function(done) {
    var xargs   = {resolve: {creator: {format: "short"}, likes: {format: "short"}}};
    var req     = helpers.basicRequestObject(xargs);
    var result;
    var error;

    before(function(done) {
      new Twine('Post', {creator: mark._id, likes_count: {$gt: 0}}, req, null, function(err, res) {
        error = err;
        result = res;
        done();
      });
    });

    it('should resolve the likes property', function(done) {
      var users = result.resolve.User;
      var like1 = result.data[0].likes[0];
      var like2 = result.data[1].likes[0];
      assert.property(users, like1._id);
      assert.property(users, like2._id);
      done();
    });

    it('should resolve the creator property', function(done) {
      var users = result.resolve.User;
      var creator1 = result.data[0].creator.toString();
      var creator2 = result.data[1].creator.toString();
      assert.property(users, creator1);
      assert.property(users, creator2);
      done();
    });
  });

  describe('Nested Resolving', function(done) {
    var xargs = {resolve: { origin_post_id: {format: "short", resolve: {creator: {format: "short"}}}}};
    var req   = helpers.basicRequestObject(xargs);
    var result;
    var error;

    before(function(done) {
      new Twine('Post', {creator: DJ._id}, req, null, function(err, res) {
        debugger;
        result = res;
        error = err;
        done();
      });
    });
    
    it('should not return an error', function(done) {
      assert.isFalse(error);
      done();
    });

    it('should return a Post Object', function(done) {
      assert.property(result.resolve, 'Post');
      assert.isObject(result.resolve.Post);
      done();
    });

    it('should return a User Object', function(done) {
      assert.property(result.resolve, 'User');
      assert.isObject(result.resolve.User);
      done();
    });

    it('should have a Post object key equal to origin_post_id', function(done) {
      var origin_post_id = result.data[0].origin_post_id;
      var post_object_key = _.keys(result.resolve.Post)[0];
      assert.equal(post_object_key, origin_post_id);
      done();
    });

    it('should have a User object key equal to Post object creator', function(done) {
      var post_key = _.keys(result.resolve.Post)[0];
      var user_key = _.keys(result.resolve.User)[0];
      var origin_post_creator = result.resolve.Post[post_key].creator.toString();
      assert.equal(user_key, origin_post_creator);
      done();
    });
  });
  
  describe('getDistinctValuesForField()', function(done) {
    var users, id, field;

    before(function(done) {
      users = [DJ, mark];
      field = "following";
      id = "_id";
      done();
    });

    it('should test', function(done) {
      var distinct_test = Twine.prototype.getDistinctValuesForField(users, id, field);
      done();
    });
  });
});
