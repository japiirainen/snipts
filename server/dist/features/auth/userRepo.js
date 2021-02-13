"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.findUserById = exports.findUserByEmail = exports.findUserByUsername = exports.insertUser = void 0;
const A = __importStar(require("fp-ts/Array"));
const db_1 = require("../../infrastructure/db");
const insertUser = (dto, pool) => db_1.withConn(pool, conn => conn
    .query('INSERT INTO users (username, email, password) VALUES ($1, $2, $3) RETURNING *', [
    dto.username,
    dto.email,
    dto.password,
])
    .then(v => {
    console.log(v);
    return v;
})
    .then(res => A.head(res.rows)));
exports.insertUser = insertUser;
const findUserByUsername = (username, pool) => db_1.withConn(pool, conn => conn
    .query('SELECT * FROM users WHERE username = $1 LIMIT 1', [username])
    .then(res => A.head(res.rows)));
exports.findUserByUsername = findUserByUsername;
const findUserByEmail = (email, pool) => db_1.withConn(pool, conn => conn
    .query('SELECT * FROM users where email = $1 LIMIT 1', [email])
    .then(res => A.head(res.rows)));
exports.findUserByEmail = findUserByEmail;
const findUserById = (id, pool) => db_1.withConn(pool, conn => conn.query('SELECT * FROM users where id = $1 LIMIT 1', [id]).then(res => A.head(res.rows)));
exports.findUserById = findUserById;
