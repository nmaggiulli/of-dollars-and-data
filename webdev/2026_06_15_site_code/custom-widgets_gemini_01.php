<?php

global $post;

add_action( 'widgets_init', 'a2_widgets' );


function a2_widgets() {
    register_widget( 'A2_Networks_Widget' );
    register_widget( 'A2_IC_Ad_Widget' );
    register_widget( 'Hot_Links_Widget' );
    // register_widget( 'Best_Of_Widget' );
}

class A2_Networks_Widget extends WP_Widget {
    
    public function __construct() {
        $widget_ops = array( 
            'classname' => 'a2_networks widget', 
            'description' => __('Display As Seen On Networks', 'a2_networks') 
        );
        $control_ops = array( 'width' => 300, 'height' => 350, 'id_base' => 'a2_networks-widget' );
        parent::__construct( 'a2_networks-widget', __('Aytoo Networks', 'custom'), $widget_ops, $control_ops );
    }

    function widget( $args, $instance ) {
        extract( $args );
        //Our variables from the widget settings.
        $location = isset($instance['location']) ? $instance['location'] : '';
        $networks = get_field('networks', 'option');

        if($networks) {
            echo $before_widget;
                echo '<div class="network-box clearfix">';
                    echo '<div class="as-seen-on">
                            <h3>As Seen On</h3>
                            <div class="networks slick">';
                                foreach($networks as $network) {
                                    echo '<div class="network"><img src="'. $network["logo"] .'"/></div>';
                                }
                            echo '</div>
                        </div>
                    </div>';
            echo $after_widget;
        }  
    }

    //Update the widget
    function update( $new_instance, $old_instance ) {
        $instance = $old_instance;

        $instance['location'] = strip_tags( $new_instance['location'] );

        return $instance;
    }

    function form( $instance ) {
        $defaults = array( 'location' => __("300x250", 'example'));
        $instance = wp_parse_args( (array) $instance, $defaults );

        ?>

        <!-- Title -->
        <p>
            <a href="/wp-admin/admin.php?page=acf-options">Manage Logos Here</a>
        </p>


        <?php
    }
} // End Widg

class A2_IC_Ad_Widget extends WP_Widget {
    
    public function __construct() {
        $widget_ops = array( 
            'classname' => 'a2_ad widget', 
            'description' => __('Display an Investing Channel Ad', 'a2_ad') 
        );
        $control_ops = array( 'width' => 300, 'height' => 350, 'id_base' => 'a2_ad-widget' );
        parent::__construct( 'a2_ad-widget', __('Aytoo Ad Display', 'custom'), $widget_ops, $control_ops );
    }

    function widget( $args, $instance ) {
        extract( $args );
        //Our variables from the widget settings.
        $location = isset($instance['location']) ? $instance['location'] : '';
            
        if(function_exists('a2_displayAd') && a2_displayAd($location)) {
            echo $before_widget;
                echo a2_displayAd($location);
            echo $after_widget;
        }  
    }

    //Update the widget
    function update( $new_instance, $old_instance ) {
        $instance = $old_instance;

        $instance['location'] = strip_tags( $new_instance['location'] );

        return $instance;
    }

    function form( $instance ) {
        $defaults = array( 'location' => __("300x250", 'example'));
        $instance = wp_parse_args( (array) $instance, $defaults );

        ?>

        <!-- Title -->
        <p>
            <label for="<?php echo $this->get_field_id( 'location' ); ?>"><?php _e('Location:', 'example'); ?></label>
            <input id="<?php echo $this->get_field_id( 'location' ); ?>" name="<?php echo $this->get_field_name( 'location' ); ?>" value="<?php echo esc_attr($instance['location']); ?>" style="width:100%;" />
        </p>


        <?php
    }
} // End Widg

class Hot_Links_Widget extends WP_Widget {
    
    public function __construct() {
        $widget_ops = array( 
            'classname' => 'hot_links widget', 
            'description' => __('Display the links from the last Hot Links post', 'hot_links') 
        );
        $control_ops = array( 'width' => 300, 'height' => 350, 'id_base' => 'hot_links-widget' );
        parent::__construct( 'hot_links-widget', __('Aytoo Recent Links', 'custom'), $widget_ops, $control_ops );
    }

    function widget( $args, $instance ) {
        extract( $args );
        //Our variables from the widget settings.
        $title = isset($instance['title']) ? $instance['title'] : '';

        $query_args = array(
            'posts_per_page' => 1,
            'category_name' => 'hot-links',
            'orderby' => 'date',
            'order' => 'DESC'
        );
        $links_query = new WP_Query( $query_args );

        if ( $links_query->have_posts() ) {
            
            echo $before_widget;

            while ( $links_query->have_posts() ) {
                $links_query->the_post();
                
                $links = array();
                if(get_field('hot_links')) {
                    $links = array_slice(get_field('hot_links'), 0, 5, true);
                }
                
                if($links) {

                    echo "<div class='inner-widget'>";
                        echo '<h3 class="widget-title"><a href="'. get_the_permalink() .'">'. esc_html($title) .'</a></h3>';

                            echo "<ul class='links'>";
                            foreach($links as $link) {
                                echo '<li class="link"> <a href="'. $link['link'] .'" target="_blank">'. $link['title'] .'</a> <span class="link-source">('. $link['source'] .')</span>';
                            }
                            echo "</ul>";
                            
                    echo "</div>";

                }
            }

            echo $after_widget;
            wp_reset_postdata();
        }

        
    }

    //Update the widget
    function update( $new_instance, $old_instance ) {
        $instance = $old_instance;

        $instance['title'] = strip_tags( $new_instance['title'] );

        return $instance;
    }

    function form( $instance ) {
        $defaults = array( 'title' => __("Today's Hot Links", 'example'));
        $instance = wp_parse_args( (array) $instance, $defaults );

        ?>

        <!-- Title -->
        <p>
            <label for="<?php echo $this->get_field_id( 'title' ); ?>"><?php _e('Title:', 'example'); ?></label>
            <input id="<?php echo $this->get_field_id( 'title' ); ?>" name="<?php echo $this->get_field_name( 'title' ); ?>" value="<?php echo esc_attr($instance['title']); ?>" style="width:100%;" />
        </p>


        <?php
    }
} // End Widg


class Best_Of_Widget extends WP_Widget {
    
    public function __construct() {
        $widget_ops = array( 
            'classname' => 'best_of widget', 
            'description' => __('Display the 5 most recent posts marked with best of', 'best_of') 
        );
        $control_ops = array( 'width' => 300, 'height' => 350, 'id_base' => 'best_of-widget' );
        parent::__construct( 'best_of-widget', __('Aytoo Best Of Posts', 'custom'), $widget_ops, $control_ops );
    }

    function widget( $args, $instance ) {
        extract( $args );
        //Our variables from the widget settings.
        $title = isset($instance['title']) ? $instance['title'] : '';

        $query_args = array(
            'posts_per_page' => 5,
            'meta_query' => array(
                array(
                    'key' => 'best_of', 
                    'value' => '"true"',
                    'compare' => 'LIKE'
                )
            ),
            'orderby' => 'date',
            'order' => 'DESC' 
        );
        $best_query = new WP_Query( $query_args );

        echo $before_widget;

        if ( $best_query->have_posts() ) {
            echo '<h3 class="widget-title">'. esc_html($title) .'</h3>';
            echo "<div class='inner-widget'>";
                echo '<ul class="best-of-list links">';
                while ( $best_query->have_posts() ) {
                    $best_query->the_post();

                    echo '<li class="link"><a href="'. get_the_permalink() .'">'. get_the_title() .'</a> <span class="link-source">'.get_the_date('M Y').'</span></li>';
                }
            echo '</ul>';

            if( get_page_by_path('best-of') ) { echo '<a class="excerpt-read-more" href="/best-of/">See All</a>'; }

            echo "</div>";
        }

        echo $after_widget;
        wp_reset_postdata();
    }

    //Update the widget
    function update( $new_instance, $old_instance ) {
        $instance = $old_instance;

        $instance['title'] = strip_tags( $new_instance['title'] );

        return $instance;
    }

    function form( $instance ) {
        $defaults = array( 'title' => __("Best Of TRB", 'example'));
        $instance = wp_parse_args( (array) $instance, $defaults );

        ?>

        <!-- Title -->
        <p>
            <label for="<?php echo $this->get_field_id( 'title' ); ?>"><?php _e('Title:', 'example'); ?></label>
            <input id="<?php echo $this->get_field_id( 'title' ); ?>" name="<?php echo $this->get_field_name( 'title' ); ?>" value="<?php echo esc_attr($instance['title']); ?>" style="width:100%;" />
        </p>


        <?php
    }
} // End Widg

?>