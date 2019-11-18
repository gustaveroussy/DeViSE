/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
module.exports = function (grunt) {
	
	grunt.loadNpmTasks('grunt-war');
    // Project configuration.
    grunt.initConfig({
    	pkg: grunt.file.readJSON('package.json'),
        war: {
            target: {
                options: {
                    war_dist_folder: 'd:', /* Folder where to generate the WAR. */
                    war_name: 'VariantDiag##Angular3'                    /* The name fo the WAR file (.war will be the extension) */
                },
                files: [
                    {
                        expand: true,
                        cwd: 'dist',
                        src: ['**'],
                        dest: ''
                    }
                ]
            }
        }
    });
    grunt.registerTask('default', ['war']);
};
