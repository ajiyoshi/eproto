-record(filedescriptorset, {
							file
						   }).

-record(filedescriptorproto, {
							name,
							package,
							dependency,
							message_type,
							enum_type,
							service,
							extension,
							options
						   }).

-record(descriptorproto, {name,
						  field,
						  extension,
						  nested_type,
						  enum_type,
						  extension_range,
						  options					
						  }).

-record(fielddescriptorproto, {
							   name,
							   number,
							   label,
							   type,
							   type_name,
							   extendee,
							   default_value,
							   options
							   }).