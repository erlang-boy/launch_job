PROJECT = launch_job
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

LOCAL_DEPS = runtime_tools
DEPS = lager cowboy eep folsom erlsh grapherl live
dep_cowboy_commit = master
export SERVERPATH=$(PWD)

ERLC_OPTS = +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1234}'


EUNIT_OPTS = verbose
EUNIT_ERL_OPTS = -args_file rel/vm.args -config rel/sys.config

SHELL_DEPS = kjell
# SHELL_ERL = $(DEPS_DIR)/kjell/bin/kjell
SHELL_OPTS = -name launch_job_shell@127.0.0.1 -setcookie launch_job

include erlang.mk
