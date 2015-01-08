module Nagios.Check.RabbitMQ.Options where

import Nagios.Check.RabbitMQ.Types

import Options.Applicative


parseOptions :: IO CheckOptions
parseOptions = execParser optionParser


optionParser :: ParserInfo CheckOptions
optionParser =
    info (helper <*> checkOptions)
    (fullDesc <>
        progDesc "Nagios NRPE check for RabbitMQ queue length" <>
        header "nagios-check-rabbitmq-queue-length - checks queue length against threshold"
    )

checkOptions :: Parser CheckOptions
checkOptions = CheckOptions
    <$> strOption
        (long "hostname"
        <> short 'h'
        <> value "localhost"
        <> metavar "HOSTNAME")
    <*> (read <$> strOption
        (long "queue"
        <> short 'q'
        <> help "Name of the queue to check"))
