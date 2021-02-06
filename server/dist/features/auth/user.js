"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.toPublicUser = void 0;
const toPublicUser = (user) => ({
    username: user.username,
    createdOn: user.created_on,
});
exports.toPublicUser = toPublicUser;
