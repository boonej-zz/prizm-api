var uuid = require('node-uuid');
var AWS = require('aws-sdk');
var gm = require('gm');
var mime = require('mime');
var multiparty = require('multiparty');
var moment    = require('moment');
var monowrap = require('monowrap');

var IMAGE_FORMATS = {
  standard: { width: 1200, height: 1200 },
  thumbLg:  { width: 600,  height: 600 },
  thumbMd:  { width: 300,  height: 300 },
  thumbSm:  { width: 80,   height: 80 }
}

var AWS_BUCKET = 'higheraltitude.prism'

var AMAZON_BASE = 'https://s3.amazonaws.com/' + AWS_BUCKET + '/';
// var AWS_BUCKET = 'higheraltitude.prizm.test'
var longest = function(text){
  var strings = text.split(' ');
  var max = 0;
  for (var i = 0; i != strings.length; ++i){
    max = strings[i].length > max?strings[i].length:max;
  };
  return max;
};

var formatText = function(text, size, next){
  text = String(text);
  var hard_max = 600/(0.5 * size);
  var longestWord = longest(text);
  if (longestWord > hard_max && size > 8) {
    formatText(text, size - 2, next);
  } else {
    var len = text.length;
    var lines = [len, len/2, len/3, len/4, len/5];
    var line = false;
    console.log(hard_max);
    console.log(lines);
    for (var i = 0; i != lines.length; ++i){
      if (lines[i] < hard_max) {
        line = true;
      }
    }
    if (line || size < 12) {
      var t = monowrap(text, {width: hard_max, tabWidth: 4, eol: '\n', top: 0,
         bottom: 0});
      next(t, size);
    } else {
      formatText(text, size - 2, next);
    }
  }
}

var processImage = function(i, p, fields, next, c){
  var s3 = new AWS.S3();
  var path = p.path;
  var q = 1;
  if (c) {
    q = Math.pow(2, c);
    path = path + '_' + String(q);
  } else {
    c = 0;
  }
  var $i = i;
  gm(i.path).size(function(err, d){
    if (d){
    var min = d.width < d.height?d.width:d.height;
    var target = min/q;
    gm(i.path)
    .gravity('Center')
    .crop(min, min)
    .resize(target, target)
    .stream(function(err, o, e){
      var b = new Buffer('');
      o.on('data', function (d) {
        b = Buffer.concat([b, d]);
      });
      o.on('end', function(d){
        var data = {
          Bucket: AWS_BUCKET,
          Key:  path + p.ext,
          Body: b,
          ContentType: mime.lookup('img' + p.ext),
          ACL: 'public-read'
        };
        s3.putObject(data, function (err, result) {
          if (p.thumbs && p.thumbs > 0) {
            if(c++ < p.thumbs) {
              processImage($i, p, fields, next, c);
            } else {
              next(err, p.path + p.ext, fields);
            }
          } else {
            next(err, p.path + p.ext, fields);
          }
        }); 
      });
    });
    }
  });
};

exports.saveTextImage = function(post, text, next){
  var p = {
    path: String(uuid.v1()),
    ext: ".jpg",
    thumbs: 2
  };
  var s3 = new AWS.S3();
  var fs = require('fs');
  var save = function(file, path, ext){
    var data = {
      Bucket: AWS_BUCKET,
      Key:  path + ext,
      Body: file,
      ContentType: mime.lookup('img' + ext),
      ACL: 'public-read'
    };
    s3.putObject(data, function (err, result) {
      if (err) console.log(err);
      else console.log(result);
    }); 
  };
  var ip = __dirname + '/../../assets/prizmcard.png';
  formatText(text, 60, function(text, size){
    var tmp = __dirname + '/../../tmp/' + String(uuid.v1()) + p.ext;
    gm(ip)
    .font('HelveticaNeueThin.ttf')
    .fill('rgb(192,193,213)')
    .fontSize(size)
    .stroke('transparent')
    .drawText(0, 0, String(text), 'center')
    .write(tmp, function(){
      var width = 640;
      for (var i = 0; i != 3; ++i){
        var q = Math.pow(2, i);
        var size = width/q;
        gm(tmp) 
        .resize(size, size)
        .stream(function(err, o, e){
          var $q = q;
          var b = new Buffer('');
          o.on('data', function(d){
            b = Buffer.concat([b, d]);
          });
          o.on('end', function(d){
            var path = p.path;
            if ($q > 1) {
              path = path + '_' + $q;
            } 
            save(b, path, p.ext);
            if ($q == 2) {
              fs.unlinkSync(tmp);
            }
          });
        });
      }
      var url = 'https://s3.amazonaws.com/' + AWS_BUCKET 
                    + '/' + p.path + p.ext;
      next(null, url);
      // fs.unlinkSync(tmp);
    });

  });

};

var processText = function(p, fields, next, c){
  var s3 = new AWS.S3();
  var fs = require('fs');
  var save = function(file, path, ext){
    var data = {
      Bucket: AWS_BUCKET,
      Key:  path + ext,
      Body: file,
      ContentType: mime.lookup('img' + ext),
      ACL: 'public-read'
    };
    s3.putObject(data, function (err, result) {
      if (err) console.log(err);
    }); 
  };
  var ip = __dirname + '/../../assets/prizmcard.png';
  formatText(fields.text, 60, function(text, size){
    var tmp = __dirname + '/../../tmp/' + String(uuid.v1()) + p.ext;
    console.log(tmp);
    gm(ip)
    .font('HelveticaNeueThin.ttf')
    .fill('rgb(192,193,213)')
    .fontSize(size)
    .stroke('transparent')
    .drawText(0, 0, String(text), 'center')
    .write(tmp, function(){
      var width = 640;
      for (var i = 0; i != 3; ++i){
        var q = Math.pow(2, i);
        var size = width/q;
        gm(tmp) 
        .resize(size, size)
        .stream(function(err, o, e){
          var $q = q;
          var b = new Buffer('');
          o.on('data', function(d){
            b = Buffer.concat([b, d]);
          });
          o.on('end', function(d){
            var path = p.path;
            if ($q > 1) {
              path = path + '_' + $q;
            } 
            save(b, path, p.ext);
            if ($q == 2) {
              fs.unlinkSync(tmp);
            }
          });
        });
      }
      next(null, p.path + p.ext );
      // fs.unlinkSync(tmp);
    });
    /** .stream(function(err, o, e){
      var b = new Buffer('');
      o.on('data', function (d) {
        b = Buffer.concat([b, d]);
      });
      o.on('end', function(d){
        for (var i = 0; i != 3; ++i) {
          var width = 640;
          if (i > 0) {
            var q = Math.pow(2, i);
            var size = width/q;
            gm(AMAZON_BASE + p.path + p.ext).resize(size, size)
            .stream(function(err, os, es){
              if (err) console.log(err);
              var buf = new Buffer('');
              os.on('data', function(bit){
                Buffer.concat([buf, bit]);
              });
              os.on('end', function(){
                save(buf, p.path + '_' + q, p.ext); 
              });
            });

          } else {
            save(b, p.path, p.ext);
          }
        }
        next(err, p.path + p.ext, fields);
      }); 
    }); **/
  });
}

exports.uploadPost = function(req, settings, next) {
  var uploadImage = function (img, path, ext, fields, next) {
    var props = {
      path: path,
      ext: ext,
      thumbs: 2
    };
    processImage(img, props, fields, next);
  };
  var uploadText = function(path, ext, fields, next) {
    var props = {
      path: path,
      ext: ext,
      thumbs: 2
    };
    processText(props, fields, next);
  };
  var user = req.user;
  var date = moment().format('ssmmhhDDMMYYYY');
  var ext = '.jpg';
  var path =  user._id + '/' + date + uuid.v1();
  var form = new multiparty.Form();
  var onComplete = function(err, path, fields){
    var url = 'https://s3.amazonaws.com/' + AWS_BUCKET 
                    + '/' + path;
    next(err, url, fields);
  };
  form.parse(req, function (err, fields, files) {
    var img = files.image;
    console.log(img);
    if (img.length > 0 && img[0].size > 0){
      uploadImage(img[0], path, ext, fields, onComplete); 
    } else {
      uploadText(path, ext, fields, onComplete);
    } 
  });
};

exports.uploadImage = function(req, res, settings, next) {
  var s3 = new AWS.S3();
  var form = new multiparty.Form();
  var date = moment().format('ssmmhhDDMMYYYY');
  var unique = uuid.v1();
  var fileExt = '.jpg';
  var fileName = date + unique + fileExt;
  var folder;
  var userId;
  var imageType;

  if (!settings) {
    next('Photo not uploaded, missing settings');
  }
  else {
    userId = settings['userId'];
    imageType = settings['imageType'];
  }

  if (!userId) {
    next('Need to provide userId to upload photo');
  }

  if (imageType == 'profile') {
    folder = 'profile/';
  }
  else {
    folder = userId + '/';
  }

  form.parse(req, function (err, fields, files) {
    var imageUrl  = 'https://s3.amazonaws.com/' + AWS_BUCKET 
                    + '/' + folder + fileName;
    var targetWidth = Number(fields.width) > 0?Number(fields.width) * 2:600;
    var targetHeight = Number(fields.height) > 0?Number(fields.height) *2: 600;
    if (files) {
      var fa = files.image;
      if (fa) {
        var file;
        for (var i = 0; file = fa[i]; ++i) {
          if (!file.originalFilename) {
            next('Image upload can not be empty');
          }
          var $f = file;
          gm(file.path).size(function(err, d){
            height = d.height;
            width = d.width;
            if (targetWidth == targetHeight) {
              width = width <= height?width:height;
              height = height <=width?height:width;
            } else {
              diffx = targetWidth/targetHeight;
              height = width/diffx;
            }
            gm($f.path)
            .gravity('Center')
            .crop(width, height)
            .resize(targetWidth, targetHeight)
            .stream(function (err, stdout, stderr) {
              var buf = new Buffer('');
              stdout.on('data', function (data) {
                buf = Buffer.concat([buf, data]);
              });
              stdout.on('end', function (data) {
                var data = {
                  Bucket: AWS_BUCKET,
                  Key: folder + fileName,
                  Body: buf,
                  ContentType: mime.lookup(fileName),
                  ACL: 'public-read'
                };
                s3.putObject(data, function (err, result) {
                  if (err) {
                    next({error: err});
                  }
                  if (result) {
                    next(null, imageUrl);
                  }
                });
              });
            });
          });
        }
      }
    }
  });
}
