import { Component, model, output } from '@angular/core';
import { Kolegij } from '../../core';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-course-edit',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './course-edit.component.html',
  styleUrl: './course-edit.component.scss',
})
export class CourseEditComponent {
  data = model.required<Kolegij>();
  query = output<Kolegij>();

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }
}
