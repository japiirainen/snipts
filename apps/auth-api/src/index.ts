import { config } from './infrastructure/config'
import { createApp } from './app'
import { logger } from './infrastructure/logger'

createApp().then(app =>
   app.listen(config.application.port, () =>
      logger.info(`${config.application.name} is listening on ${config.application.port}`)
   )
)
