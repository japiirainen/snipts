"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.toPublicUser = void 0;
var toPublicUser = function (user) { return ({
    username: user.username,
    createdOn: user.created_on,
}); };
exports.toPublicUser = toPublicUser;
