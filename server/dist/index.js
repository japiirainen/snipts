"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const cookie_parser_1 = __importDefault(require("cookie-parser"));
const cors_1 = __importDefault(require("cors"));
const express_1 = __importDefault(require("express"));
const morgan_1 = __importDefault(require("morgan"));
const authMiddleware_1 = require("./features/auth/authMiddleware");
const authRoutes_1 = require("./features/auth/authRoutes");
const config_1 = require("./infrastructure/config");
const db_1 = require("./infrastructure/db");
const env_1 = require("./infrastructure/env");
const logger_1 = require("./infrastructure/logger");
const createApp = async () => {
    const prerequisites = [db_1.createDbPool(), Promise.resolve()];
    const [pool] = await Promise.all(prerequisites);
    const app = express_1.default();
    app.use(morgan_1.default('dev'))
        .use(cors_1.default({
        credentials: true,
        origin: 'http://localhost:4000',
    }))
        .use(express_1.default.json())
        .use(cookie_parser_1.default())
        .get('/health', (_, res) => pool
        ? res.status(200).json({ status: 'healthy' })
        : res.status(503).json({ status: 'unavailable' }))
        .use(env_1.initializeEnv(pool))
        .get('/me', authMiddleware_1.requireUser, authRoutes_1.authRoutes.me)
        .post('/login', authRoutes_1.authRoutes.login)
        .post('/logout', authMiddleware_1.requireUser, authRoutes_1.authRoutes.logout)
        .post('/register', authRoutes_1.authRoutes.register)
        .post('/refresh-token', authRoutes_1.authRoutes.refreshToken);
    return app;
};
createApp().then(app => app.listen(config_1.config.application.port, () => logger_1.logger.info(`${config_1.config.application.name} is listening on ${config_1.config.application.port}`)));
