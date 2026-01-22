#pragma once
#include <string>

#include <FL/Fl_PNG_Image.H>
#include <FL/fl_draw.H>

#include "Model.h"

class CardWidget : public Fl_Widget {
    Fl_PNG_Image* img;
    std::string text;

public:
    CardWidget(int X, int Y, int W, int H, const char* L = 0)
        : Fl_Widget(X, Y, W, H, L), img(nullptr) {
    }

    void set_model(const CardModel& model) {
        if (img) delete img;
        img = new Fl_PNG_Image(model.imageFile.c_str());
        if (model.atk != -1)
            text = (model.caption + " " + std::to_string(model.atk)).c_str();
        else
            text = model.caption;
    }

    void draw() override {
        if (img) {
            int img_h = h() * 2 / 3;
            img->draw(x(), y(), w(), img_h);
        }
        int text_y = y() + h() * 2 / 3 + 20;
        fl_color(FL_BLACK);
        fl_font(FL_HELVETICA, 16);
        fl_draw(text.c_str(), x() + 10, text_y);
    }

    int handle(int event) override {
        if (event == FL_PUSH && Fl::event_button() == FL_LEFT_MOUSE) {
            do_callback();
            return 1;
        }
        return Fl_Widget::handle(event);
    }
};
